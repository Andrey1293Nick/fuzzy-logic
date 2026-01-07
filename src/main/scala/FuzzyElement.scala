package hidewise.fuzzy_logic

// todo сделать тип с валидацией
opaque type FuzzyValue = Double
opaque type FuzzyElement[+A] = (A, FuzzyValue)

object FuzzyValue {

  val ZERO: FuzzyValue = 0.0
  val ONE:  FuzzyValue = 1.0

  @inline def apply(value: Double): Either[InvalidFuzzyValue, FuzzyValue] =
    value match {
      case v if v >= 0 && v <= 1 => Right(value)
      case v if v < 0            => Left(InvalidFuzzyValue.LessZero)
      case v if v > 1            => Left(InvalidFuzzyValue.GreaterOne)
    }

  @inline private[fuzzy_logic] def unsafe(value: Double): FuzzyValue = value

  extension (fValue: FuzzyValue)
    @inline def unwrap: Double = fValue
    @inline def >(other: FuzzyValue): Boolean = fValue > other
    @inline def <(other: FuzzyValue): Boolean = fValue < other
    @inline def -(other: Double): FuzzyValue = fValue - other

}

sealed trait InvalidFuzzyValue

object InvalidFuzzyValue:

  case object LessZero   extends InvalidFuzzyValue
  case object GreaterOne extends InvalidFuzzyValue

end InvalidFuzzyValue

object FuzzyElement {

  @inline def apply[A](value: (A, FuzzyValue)): FuzzyElement[A] = value

  extension [A](e: FuzzyElement[A])
    @inline def value: A = e._1
    @inline def membership: FuzzyValue = e._2
    @inline def negate: FuzzyElement[A] = FuzzyElement(e.value, 1.0 - e.membership)
    @inline def unary_! : FuzzyElement[A] = negate

}
