package hidewise.fuzzy_logic

sealed trait FuzzyCollection[+A]

case object FEmpty extends FuzzyCollection[Nothing]

class FCons[A](
  private val _head: FuzzyElement[A],
  private val _tail: FuzzyCollection[A]
) extends FuzzyCollection[A] with Seq[FuzzyElement[A]] {

  private val _iterator: Iterator[FuzzyElement[A]] = new Iterator[FuzzyElement[A]] {
    private var current: FuzzyCollection[A] = FCons.this

      override def hasNext: Boolean = _tail match {
        case FEmpty => false
        case cons: FCons[_] => true
      }

      override def next(): FuzzyElement[A] = _tail match {
        case FEmpty => throw new NoSuchElementException("next on empty iterator")
        case cons: FCons[A] =>
          current = cons._tail
          cons._head
      }
    }

  override def length: Int = _tail match {
    case FEmpty => 1
    case cons: FCons[A] => 1 + cons.length
  }

  override def iterator: Iterator[FuzzyElement[A]] = _iterator

  override def apply(i: Int): FuzzyElement[A] =
    if (i < 0) throw new IndexOutOfBoundsException(i)
    else if (i == 0) _head
    else _tail match {
      case FEmpty => throw new IndexOutOfBoundsException(i)
      case cons: FCons[A] => cons(i - 1)
    }
}


object FuzzyCollection{

  extension(seq: Seq[Double])
    def toFuzzy(mFunction: MembershipFunction) = seq.foldLeft[FuzzyCollection[Double]](FEmpty){ case (acc, value) =>
      FCons(value -> mFunction(value), acc)
    }

  extension[A](seq: Seq[A])
    def toFuzzy(f: A => Double)(mFunction: MembershipFunction): FuzzyCollection[A] =
      seq.foldLeft[FuzzyCollection[A]](FEmpty){ case (acc, value) =>
        FCons(value -> mFunction(f(value)), acc)
      }

  def fuzzify[A](value: Double, mFunction: MembershipFunction): FuzzyValue = mFunction(value)
}