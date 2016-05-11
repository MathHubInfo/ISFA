import org.scalatest.FlatSpec
import parser._
import relations.GeneratingFunctionSearch._

/**
 * Created by enxhell on 4/17/16.
 */
class TransformationTests extends FlatSpec{


  "constant removings" should "be transform to" in {
    val expr1 = Div(Num(3) :: Var("x") :: Nil)
    val result1 = Div(Num(1)::Var("x")::Nil)

    assert(removeConstants(expr1) == result1)
  }

  "constant removings neg" should "be transform to" in {
    val expr1 = Div(Neg(Num(3)) :: Var("x") :: Nil)
    val result1 = Div(Num(1)::Var("x")::Nil)

    assert(removeConstants(expr1) == result1)
  }

  "constant removings1" should "be transform to" in {
    val expr1 = Div(Num(3) :: Num(4) :: Var("x") :: Nil)
    val result1 = Div(Num(1)::Var("x")::Nil)

    assert(removeConstants(expr1) == result1)
  }

  "constant removings2" should "be transform to" in {
    val expr1 = Mul(Num(3) :: Var("x") :: Mul(Num(5)::Var("x")::Nil) :: Nil)
    val result1 = Mul(Var("x") :: Mul(Num(5)::Var("x")::Nil) :: Nil)

    assert(removeConstants(expr1) == result1)
  }

  "constant removings3" should "be transform to" in {
    val expr1 = Mul(Num(3) :: Num(4) :: Var("x") :: Nil)
    val result1 = Var("x")

    assert(removeConstants(expr1) == result1)
  }

  "x removings1" should "transform" in {
    val expr = Div(Var("x") :: Mul(Num(4)::Nil)::Nil)
    val result = Div(Num(1)::Mul(Num(4)::Nil)::Nil)

    assert(removeXMultiplications(expr) == result)
  }

  "x removings2" should "transform" in {
    val expr = Div(Power(Var("x"), Num(5)) :: Mul(Num(4)::Nil)::Nil)
    val result = Div(Num(1)::Mul(Num(4)::Nil)::Nil)

    assert(removeXMultiplications(expr) == result)
  }

  "x removings combined" should "transform" in {
    val expr = Div(Mul(Num(5)::Power(Var("x"), Num(5))::Nil) :: Mul(Num(4)::Nil)::Nil)
    val result = Div(Num(1)::Mul(Num(4)::Nil)::Nil)

    assert(removeXMultiplications(removeConstants(expr)) == result)
  }

  "fraction transform" should "transform" in {
    val expr = Div(Num(1) :: Sub(Num(1)::Mul(Num(5)::Var("x")::Nil)::Nil)::Nil)
    val result = Div(Num(1) :: Sub(Num(1)::Var("x")::Nil)::Nil)

    assert(normalizeFractions(removeXMultiplications(removeConstants(expr))) == result)
  }

  "fraction transform combined" should "transform" in {
    val expr = Div(Num(1) :: Power(Sub(Num(1)::Mul(Num(5)::Var("x")::Nil)::Nil), Num(5))::Nil)
    val result = Div(Num(1) :: Sub(Num(1)::Var("x")::Nil)::Nil)

    assert(normalizeFractions(removeXMultiplications(removeConstants(expr))) == result)
  }
}
