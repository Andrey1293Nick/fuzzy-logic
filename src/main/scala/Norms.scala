package hidewise.fuzzy_logic

//trait FuzzyLogic {
//  @inline def t(a: FuzzyValue, b: FuzzyValue): FuzzyValue
//  @inline def s(a: FuzzyValue, b: FuzzyValue): FuzzyValue
//
//  @inline def unary_!(a: FuzzyValue) = not(a)
//
//  @inline def not(a:     FuzzyValue): FuzzyValue = !a
//}
//
//given zadehLogic: FuzzyLogic with {
//  def t(a: FuzzyValue, b: FuzzyValue): FuzzyValue = FuzzyValue.unsafe(math.min(a.unwrap, b.unwrap))
//
//  def s(a: FuzzyValue, b: FuzzyValue): FuzzyValue = FuzzyValue.unsafe(math.max(a.unwrap, b.unwrap))
//}
//
//given productLogic: FuzzyLogic with
//  def t(a: FuzzyValue, b: FuzzyValue): FuzzyValue = FuzzyValue.unsafe(a.unwrap * b.unwrap)
//  def s(a: FuzzyValue, b: FuzzyValue): FuzzyValue = FuzzyValue.unsafe(a.unwrap + b.unwrap - a.unwrap * b.unwrap)


trait FuzzyLogic[A] {
  @inline def t(a: FuzzyElement[A], b: FuzzyElement[A]): FuzzyElement[A]
  @inline def s(a: FuzzyElement[A], b: FuzzyElement[A]): FuzzyElement[A]

  @inline def unary_!(a: FuzzyElement[A]) = not(a)

  @inline def not(a: FuzzyElement[A]): FuzzyElement[A] = !a
}

given zadehLogic[A]: FuzzyLogic[A] with {
  @inline def t(a: FuzzyElement[A], b: FuzzyElement[A]): FuzzyElement[A] = FuzzyElement(a.value, FuzzyValue.unsafe(math.min(a.membership.unwrap, b.membership.unwrap)))
  //FuzzyValue.unsafe(math.min(a.unwrap, b.unwrap))

  @inline def s(a: FuzzyElement[A], b: FuzzyElement[A]): FuzzyElement[A] = ???///FuzzyValue.unsafe(math.max(a.unwrap, b.unwrap))
}