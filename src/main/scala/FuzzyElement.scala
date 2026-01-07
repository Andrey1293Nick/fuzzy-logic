package hidewise.fuzzy_logic

// todo сделать тип с валидацией
opaque type FuzzyValue = Double
type FuzzyElement[+A] = (A, FuzzyValue)

object FuzzyValue {

  def apply(value: Double): Either[InvalidFuzzyValue, FuzzyValue] =
    value match {
      case v if v >= 0 && v <= 1 => Right(value)
      case v if v < 0            => Left(InvalidFuzzyValue.LessZero)
      case v if v > 1            => Left(InvalidFuzzyValue.GreaterOne)
    }

  private[fuzzy_logic] def unsafe(value: Double): FuzzyValue = value
}

sealed trait InvalidFuzzyValue

object InvalidFuzzyValue:

  case object LessZero   extends InvalidFuzzyValue
  case object GreaterOne extends InvalidFuzzyValue

end InvalidFuzzyValue
