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

    fuzzy.iterator.size shouldBe 5
  }

  it should "check eq seq and fuzzy" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction
    val fuzzy = seq.toFuzzy(fun)

    fuzzy.iterator.toSet.map(_.value) shouldBe seq.toSet
  }

  it should "add fuzzy collection" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val seq2 = Seq(6.0, 7.0, 8.0, 9.0, 10.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)
    val fuzzy2 = seq2.toFuzzy(fun)
    val res = fuzzy.add(fuzzy2)

    res.iterator.size shouldBe 10
    res.iterator.toSet.map(_.value) shouldBe (seq ++ seq2).toSet
  }

  it should "add" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)
    val res = fuzzy.add(FuzzyElement(6.0, FuzzyValue.ZERO))
    res.iterator.toSet.map(_.value) shouldBe (seq :+ 6.0).toSet
    res.iterator.size shouldBe 6
  }

  it should "union" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)

    val seqStr = Seq("A1","B12","C123")
    val fuzzyStr = seqStr.toFuzzy(_.length)(fun)

    val res = fuzzy.union(fuzzyStr)

    res.iterator.size shouldBe seq.size + seqStr.size
  }

  it should "map" in {
    val seq = Seq(1.0, 2.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)

    val res = fuzzy.map{(v ) =>
      FuzzyElement(s"test ${v.value}", FuzzyValue.ZERO)
    }
    res.iterator.size shouldBe 2
    res.iterator.toSeq shouldBe Seq(FuzzyElement("test 1.0",FuzzyValue.ZERO), FuzzyElement("test 2.0",FuzzyValue.ZERO))
  }
  it should "flatMap" in {
    val seq = Seq(1.0, 2.0)
    val fun = FuzzyListSpec.membershipFunction

    val fuzzy = seq.toFuzzy(fun)

    val res = fuzzy.flatMap {(v ) =>
      FuzzyList(FuzzyElement(s"test ${v.value}", FuzzyValue.ZERO), FuzzyEmptyCollection)
    }
    res.iterator.size shouldBe 2
    res.iterator.toSeq shouldBe Seq(FuzzyElement("test 1.0",FuzzyValue.ZERO), FuzzyElement("test 2.0",FuzzyValue.ZERO))
  }
}

object FuzzyListSpec {
  def membershipFunction(double: Double): FuzzyValue = FuzzyValue.unsafe(double)
}
