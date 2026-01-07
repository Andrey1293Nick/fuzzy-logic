package hidewise.fuzzy_logic

sealed trait FuzzyCollection[+A] extends Seq[FuzzyElement[A]]

object FuzzyEmptySeq extends FuzzyCollection[Nothing] {
  override def length: Int = 0

  override def iterator: Iterator[(Nothing, FuzzyValue)] = Iterator.empty

  override def apply(i: Int): (Nothing, FuzzyValue) = throw new IndexOutOfBoundsException(i)
}

class FuzzySeq[A](
  private val _head: FuzzyElement[A],
  private val _tail: FuzzyCollection[A])
    extends FuzzyCollection[A]
    with Seq[FuzzyElement[A]] {

  private val _iterator: Iterator[FuzzyElement[A]] = new Iterator[FuzzyElement[A]] {
//    private var current: FuzzyCollection[A] = FCons.this

    override def hasNext: Boolean =
      _tail match {
        case FuzzyEmptySeq     => false
        case cons: FuzzySeq[_] => true
      }

    override def next(): FuzzyElement[A] =
      _tail match {
        case FuzzyEmptySeq     => throw new NoSuchElementException("next on empty iterator")
        case cons: FuzzySeq[A] => cons._head
      }
  }

  override def length: Int =
    _tail match {
      case FuzzyEmptySeq     => 1
      case cons: FuzzySeq[A] => 1 + cons.length
    }

  override def iterator: Iterator[FuzzyElement[A]] = _iterator

  override def apply(i: Int): FuzzyElement[A] =
    _tail match {
      case _ if i < 0        => throw new IndexOutOfBoundsException(i)
      case _ if i == 0       => _head
      case cons: FuzzySeq[A] => cons(i - 1)
      case FuzzyEmptySeq     => throw new IndexOutOfBoundsException(i)
    }

  override def toString(): String = s"FuzzyElement[${_head._1}|${_head._2}]"
}

object FuzzyCollection {

  extension (seq: Seq[Double])

    def toFuzzy(mFunction: MembershipFunction) =
      seq.foldLeft[FuzzyCollection[Double]](FuzzyEmptySeq) { case (acc, value) =>
        println(acc)
        FuzzySeq(value -> mFunction(value), acc)
      }

  extension [A](seq: Seq[A])

    def toFuzzy(f: A => Double)(mFunction: MembershipFunction): FuzzyCollection[A] =
      seq.foldLeft[FuzzyCollection[A]](FuzzyEmptySeq) { case (acc, value) =>
        FuzzySeq(value -> mFunction(f(value)), acc)
      }

  def fuzzify[A](value: Double, mFunction: MembershipFunction): FuzzyValue = mFunction(value)
}
