package hidewise.fuzzy_logic

import FuzzyCollection.toFuzzy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.*

//todo changed naming
class FuzzyListSpec extends AnyFlatSpec with Matchers {

  it should "length check" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction
    val fuzzy = seq.toFuzzy(fun)

    fuzzy.size shouldBe 5
  }

  it should "check eq seq and fuzzy" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction
    val fuzzy = seq.toFuzzy(fun)

    fuzzy.toSet.map(_.value) shouldBe seq.toSet
  }

  it should "add fuzzy collection" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val seq2 = Seq(6.0, 7.0, 8.0, 9.0, 10.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)
    val fuzzy2 = seq2.toFuzzy(fun)
    val res = fuzzy.add(fuzzy2)

    res.size shouldBe 10
    res.toSet.map(_.value) shouldBe (seq ++ seq2).toSet
  }

  it should "add" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)
    val res = fuzzy.add(FuzzyElement(6.0, FuzzyValue.ZERO))
    res.toSet.map(_.value) shouldBe (seq :+ 6.0).toSet
    res.size shouldBe 6
  }
}

object FuzzyListSpec {
  def membershipFunction(double: Double): FuzzyValue = FuzzyValue.unsafe(double)
}
