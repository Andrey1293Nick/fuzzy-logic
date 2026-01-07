package hidewise.fuzzy_logic

import scala.annotation.tailrec

sealed trait FuzzyCollection[+A] extends Iterable[FuzzyElement[A]] {

  @inline def isNormal: Boolean

  @inline def union[B >: A](fCollection: FuzzyCollection[B]):     FuzzyCollection[B]
  @inline def intersect[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B]

  @inline def add[B >: A](elem: FuzzyElement[B]):           FuzzyCollection[B]
  @inline def add[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B]

  @inline def +[B >: A](elem:  FuzzyElement[B]):    FuzzyCollection[B] = add(elem)
  @inline def ++[B >: A](elem: FuzzyCollection[B]): FuzzyCollection[B] = add(elem)
}

case object FuzzyEmptyCollection extends FuzzyCollection[Nothing] {

  override def isNormal: Boolean                         = true
  override def iterator: Iterator[FuzzyElement[Nothing]] = Iterator.empty

  override def union[B >: Nothing](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = fCollection

  override def intersect[B >: Nothing](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = this

  override def add[B >: Nothing](elem: FuzzyElement[B]): FuzzyCollection[B] = FuzzyList(elem, FuzzyEmptyCollection)

  override def add[B >: Nothing](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = fCollection
}

class FuzzyList[A](
  private val _head: FuzzyElement[A],
  val _tail:         FuzzyCollection[A])
    extends FuzzyCollection[A] {

  override def isNormal: Boolean =
    !iterator.exists { (v: FuzzyElement[A]) =>
      v.membership > FuzzyValue.ZERO
    }

  override def iterator: Iterator[FuzzyElement[A]] =
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

  override def union[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = add(fCollection)

  override def intersect[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = ???

  override def add[B >: A](elem: FuzzyElement[B]): FuzzyCollection[B] = FuzzyList(elem, this)

  override def add[B >: A](fCollection: FuzzyCollection[B]): FuzzyCollection[B] = recursiveUnion(fCollection, this)

  @tailrec
  private def recursiveUnion[B >: A](acc: FuzzyCollection[B], list: FuzzyCollection[B]): FuzzyCollection[B] =
    list match {
      case FuzzyEmptyCollection    => acc
      case fuzzyList: FuzzyList[B] => recursiveUnion(acc.add(fuzzyList._head), fuzzyList._tail)
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
