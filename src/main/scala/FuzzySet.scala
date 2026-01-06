package hidewise.fuzzy_logic

//trait FuzzySet[-A] extends (A => FuzzyValue)
sealed trait FuzzyCollection[A]

case object FEmpty extends FuzzyCollection[Nothing]

case class FCons[A](
  head: FuzzyElement[A],
  tail: FuzzyCollection[A]
) extends FuzzyCollection[A]


object FuzzyCollection{

  extension[A](seq: Seq[Double])
    def toFuzzy(mFunction: MembershipFunction) = seq.map(mFunction)

  extension[A](seq: Seq[A])
    def toFuzzy(f: A => Double)(mFunction: MembershipFunction): Seq[FuzzyElement[A]] = seq.map( v => v -> mFunction(f(v)))

  def fuzzify[A](value: Double, mFunction: MembershipFunction): FuzzyValue = mFunction(value)
}