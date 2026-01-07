package hidewise.fuzzy_logic

sealed trait FuzzyCollection[+A] extends Iterable[FuzzyElement[A]]

object FuzzyEmptySeq extends FuzzyCollection[Nothing] {

  override def iterator: Iterator[(Nothing, FuzzyValue)] = Iterator.empty

  override def toString(): String = "FuzzyEmptySeq"
}

class FuzzyList[A](
  private val _head: FuzzyElement[A],
  private val _tail: FuzzyCollection[A])
    extends FuzzyCollection[A] {

//  override def length: Int =
//    _tail match {
//      case FuzzyEmptySeq     => 1
//      case cons: FuzzySeq[A] => 1 + cons.length
//    }

  override def iterator: Iterator[FuzzyElement[A]] = new Iterator[FuzzyElement[A]] {
    private var current: FuzzyCollection[A] = FuzzyList.this

    override def hasNext: Boolean = {
      current match {
        case _: FuzzyList[_] => true
        case FuzzyEmptySeq  => false
      }
    }

    override def next(): FuzzyElement[A] =
      current match {
        case seq: FuzzyList[A] =>
          val res = seq._head
          current = seq._tail
          res
        case FuzzyEmptySeq =>
          throw new NoSuchElementException
      }
  }

//  override def apply(i: Int): FuzzyElement[A] =
//    _tail match {
//      case _ if i < 0        => throw new IndexOutOfBoundsException(i)
//      case _ if i == 0       => _head
//      case cons: FuzzySeq[A] => cons(i - 1)
//      case FuzzyEmptySeq     => throw new IndexOutOfBoundsException(i)
//    }

  override def toString(): String = s"FuzzyElement[${_head._1}|${_head._2}]"
}

object FuzzyCollection {

  extension (seq: Seq[Double])
    def toFuzzy(mFunction: MembershipFunction) =
      seq.foldLeft[FuzzyCollection[Double]](FuzzyEmptySeq) { case (acc, value) =>
        FuzzyList(value -> mFunction(value), acc)
      }

  extension [A](seq: Seq[A])

    def toFuzzy(f: A => Double)(mFunction: MembershipFunction): FuzzyCollection[A] =
      seq.foldLeft[FuzzyCollection[A]](FuzzyEmptySeq) { case (acc, value) =>
        FuzzyList(value -> mFunction(f(value)), acc)
      }

  def fuzzify[A](value: Double, mFunction: MembershipFunction): FuzzyValue = mFunction(value)
}
