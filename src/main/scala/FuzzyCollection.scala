package hidewise.fuzzy_logic

import scala.annotation.tailrec

sealed trait FuzzyCollection[+A] /*extends Iterable[FuzzyElement[A]]*/ {

  def isNormal: Boolean

  def iterator: Iterator[FuzzyElement[A]]
  def union[B >: A](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]):     FuzzyCollection[B]
//  def union[B](fCollection: FuzzyCollection[B]):     FuzzyCollection[A | B]
  def intersect[B >: A](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[B]
//  def intersect[B](fCollection: FuzzyCollection[B]): FuzzyCollection[A & B]

  def add[B >: A](elem: FuzzyElement[B]):           FuzzyCollection[B]
  def add[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B]

  def +[B >: A](elem:  FuzzyElement[B]):    FuzzyCollection[B] = add(elem)
  def ++[B >: A](elem: FuzzyCollection[B]): FuzzyCollection[B] = add(elem)

  def map[B](f: FuzzyElement[A] => FuzzyElement[B]):        FuzzyCollection[B]
  def flatMap[B](f: FuzzyElement[A] => FuzzyCollection[B]): FuzzyCollection[B]
  def filter(f: FuzzyElement[A] => Boolean): FuzzyCollection[A]
  def complement: FuzzyCollection[A]
  def boundedSum[B >: A](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]) : FuzzyCollection[B]
}

case object FuzzyEmptyCollection extends FuzzyCollection[Nothing] {

  override def isNormal: Boolean                         = true
  def iterator: Iterator[FuzzyElement[Nothing]] = Iterator.empty

  override def union[B >: Nothing](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[B] = fCollection

  override def add[B >: Nothing](elem: FuzzyElement[B]): FuzzyCollection[B] = FuzzyList(elem, FuzzyEmptyCollection)

  override def add[B >: Nothing](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = fCollection

  override def intersect[B](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[Nothing & B] = this

  override def complement: FuzzyCollection[Nothing] = this

  override def map[B](f: FuzzyElement[Nothing] => FuzzyElement[B]): FuzzyCollection[B] = this

  override def flatMap[B](f: FuzzyElement[Nothing] => FuzzyCollection[B]): FuzzyCollection[B] = this

  override def boundedSum[B >: Nothing](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[Nothing | B] = fCollection

  override def filter(f: (FuzzyElement[Nothing]) => Boolean): FuzzyCollection[Nothing] = this
}

class FuzzyList[A](
  private val _head: FuzzyElement[A],
  private val _tail: FuzzyCollection[A])
    extends FuzzyCollection[A] {

  override def isNormal: Boolean =
    !iterator.exists { (v: FuzzyElement[A]) =>
      v.membership > FuzzyValue.ONE
    }

  def iterator: Iterator[FuzzyElement[A]] =
    new Iterator[FuzzyElement[A]] {
      private var current: FuzzyCollection[A] = FuzzyList.this

      override def hasNext: Boolean = {
        current match {
          case _: FuzzyList[_]      => true
          case FuzzyEmptyCollection => false
        }
      }

      override def next(): FuzzyElement[A] =
        current match {
          case seq: FuzzyList[A] =>
            val res = seq._head
            current = seq._tail
            res
          case FuzzyEmptyCollection =>
            throw new NoSuchElementException
        }
    }

  override def union[B >: A](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[ B] = {
    recursiveMerger(fCollection, this)
  }
//  override def union[B](fCollection: FuzzyCollection[B]): FuzzyCollection[A | B] = {
//    recursiveUnion(fCollection, this)
//  }

  override def add[B >: A](elem: FuzzyElement[B]): FuzzyCollection[B] = FuzzyList(elem, this)

  override def add[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = recursiveMerger(fCollection, this)

  // todo can add a composition function A & B?
  override def intersect[B >: A](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[ B] = ???

  override def complement: FuzzyCollection[A] = this.map((v: FuzzyElement[A]) => !v)

  override def map[B](f: FuzzyElement[A] => FuzzyElement[B]): FuzzyCollection[B] =
    recursionMap(this, FuzzyEmptyCollection, f)

  override def flatMap[B](f: FuzzyElement[A] => FuzzyCollection[B]): FuzzyCollection[B] =
    recursiveFlatMap(this, FuzzyEmptyCollection, f)

  @tailrec
  private def recursiveFlatMap[B](
    tail: FuzzyCollection[A],
    acc:  FuzzyCollection[B],
    f:    FuzzyElement[A] => FuzzyCollection[B]
  ): FuzzyCollection[B] =
    tail match {
      case FuzzyEmptyCollection => acc
      case fList: FuzzyList[A]  => recursiveFlatMap(fList._tail, f(fList._head) ++ acc, f)
    }

  @tailrec
  private def recursiveMerger[B >: A](acc: FuzzyCollection[B], list: FuzzyCollection[B]): FuzzyCollection[B] =
    list match {
      case FuzzyEmptyCollection    => acc
      case fuzzyList: FuzzyList[B] => recursiveMerger(acc.add(fuzzyList._head), fuzzyList._tail)
    }

  @tailrec
  private def recursionMap[B](
    tail: FuzzyCollection[A],
    acc:  FuzzyCollection[B],
    f:    FuzzyElement[A] => FuzzyElement[B]
  ): FuzzyCollection[B] =
    tail match {
      case FuzzyEmptyCollection => acc
      case fList: FuzzyList[A]  => recursionMap(fList._tail, FuzzyList(f(fList._head), acc), f)
    }

  override def boundedSum[B >: A](fCollection: FuzzyCollection[B])(fLogic: FuzzyLogic[B]): FuzzyCollection[B] = {

    ???
  }

  override def filter(f: FuzzyElement[A] => Boolean): FuzzyCollection[A] = {
    this.flatMap{ (v: FuzzyElement[A]) =>
      if (f(v)) this
      else FuzzyEmptyCollection
    }
  }
}

object FuzzyCollection {

  extension (seq: Seq[Double])

    def toFuzzy(mFunction: MembershipFunction) =
      seq.foldLeft[FuzzyCollection[Double]](FuzzyEmptyCollection) { case (acc, value) =>
        FuzzyList(FuzzyElement(value -> mFunction(value)), acc)
      }

  extension [A](seq: Seq[A])

    def toFuzzy(f: A => Double)(mFunction: MembershipFunction): FuzzyCollection[A] =
      seq.foldLeft[FuzzyCollection[A]](FuzzyEmptyCollection) { case (acc, value) =>
        FuzzyList(FuzzyElement(value -> mFunction(f(value))), acc)
      }

  def fuzzify[A](value: Double, mFunction: MembershipFunction): FuzzyValue = mFunction(value)
}
