package hidewise.fuzzy_logic

type MembershipFunction = Double => FuzzyValue


def y(a: Double, b: Double): MembershipFunction =
  x =>
    if x <= a then FuzzyValue.unsafe(0.0)
    else if x >= b then FuzzyValue.unsafe(1.0)
    else FuzzyValue.unsafe((x - a) / (b - a))