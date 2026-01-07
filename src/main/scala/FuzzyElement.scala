package hidewise.fuzzy_logic

// todo сделать тип с валидацией
opaque type FuzzyValue = Double
opaque type FuzzyElement[+A] = (A, FuzzyValue)

object FuzzyValue {

  def apply(value: Double): Either[InvalidFuzzyValue, FuzzyValue] =
    value match {
      case v if v >= 0 && v <= 1 => Right(value)
      case v if v < 0            => Left(InvalidFuzzyValue.LessZero)
      case v if v > 1            => Left(InvalidFuzzyValue.GreaterOne)
    }

  private[fuzzy_logic] def unsafe(value: Double): FuzzyValue = value

  val ZERO: FuzzyValue = 0.0
  val ONE: FuzzyValue = 1.0

  extension(fValue: FuzzyValue)
    def unwrap: Double = fValue
    def >(other: FuzzyValue): Boolean = fValue > other
    def <(other: FuzzyValue): Boolean = fValue < other

}

sealed trait InvalidFuzzyValue

object InvalidFuzzyValue:

  case object LessZero   extends InvalidFuzzyValue
  case object GreaterOne extends InvalidFuzzyValue

end InvalidFuzzyValue

object FuzzyElement {

  def apply[A](value:(A, FuzzyValue)): FuzzyElement[A] = value

  extension [A](e: FuzzyElement[A])
    def value: A = e._1
    def membership: FuzzyValue = e._2
}