import org.scalatest.FlatSpec
import parser._

/**
 * Created by enxhi on 4/3/15.
 */

class FormulaParserTest extends FlatSpec{


  val expr1 = "T(x) + x + 3"
  val result1 = Add(List(Func("T", ArgList(List(Var("x")))), Var("x"), Num(3)))

  val expr2 = "T(x!) + x! + 3!"
  val result2 = Add(List(Func("T", ArgList(List(Factorial(Var("x"))))), Factorial(Var("x")), Factorial(Num(3))))

  val expr3 = "T(A313404(k,a)) + sum_{x^2^2 = T(x^2+3) - T(A(x)) + A301340 + A123491(n,k)}^{A131655(y)}x+2*x+binomial(x,k)"
  val result3 = Add(List(Func("T",ArgList(List(FuncR(SeqReference("A313404"),ArgList(List(Var("k"), Var("a"))))))), Iters("sum",Some(Equation("=",Power(Var("x"),Power(Num(2.0),Num(2.0))),Add(List(Sub(List(Func("T",ArgList(List(Add(List(Power(Var("x"),Num(2.0)), Num(3.0)))))), Func("T",ArgList(List(Func("A",ArgList(List(Var("x"))))))))), SeqReference("A301340"), FuncR(SeqReference("A123491"),ArgList(List(Var("n"), Var("k")))))))),Some(FuncR(SeqReference("A131655"),ArgList(List(Var("y"))))),Add(List(Var("x"), Mul(List(Num(2.0), Var("x"))), Func("binomial",ArgList(List(Var("x"), Var("k")))))))))

  val expr4 = "1 + T(x) - T^2(x)/2 + T(x^2)/2"
  val result4 = Add(List(Sub(List(Add(List(Num(1.0), Func("T",ArgList(List(Var("x")))))), Div(List(Power(Func("T",ArgList(List(Var("x")))),Num(2.0)), Num(2.0))))), Div(List(Func("T",ArgList(List(Power(Var("x"),Num(2.0))))), Num(2.0)))))

  val expr5 = "A(x)T(x) + 2*3"
  val result5 = Add(List(Mul(List(Func("A",ArgList(List(Var("x")))), Func("T",ArgList(List(Var("x")))))), Mul(List(Num(2.0), Num(3.0)))))

  val expr6 = "T(x)(x+3)"
  val result6 = Mul(List(Func("T",ArgList(List(Var("x")))), Add(List(Var("x"), Num(3.0)))))

  val expr7 = "sum_{x=1}^{t} A(x)=3 "
  val result7 = Iters("sum", Some(Equation("=",Var("x"), Num(1.0))),Some(Var("t")),Equation("=",Func("A",ArgList(List(Var("x")))), Num(3.0)))

  val expr8 = "a(n) = A001045(n)+A000035(n+1)"
  val result8 = Equation("=",Func("a",ArgList(List(Var("n")))), Add(List(FuncR(SeqReference("A001045"),ArgList(List(Var("n")))), FuncR(SeqReference("A000035"),ArgList(List(Add(List(Var("n"), Num(1.0)))))))))

  val expr9 = "sum{t=1..x^2} t+2"
  val result9 = Iters("sum", Some(Equation("=",Var("t"), Num(1.0))),Some(Power(Var("x"),Num(2.0))),Add(List(Var("t"), Num(2.0))))

  val expr10 = "(exp(2*x)-exp(-x))/3+cosh(x) = (2*exp(2*x)+3*exp(x)+exp(-x))/6"
  val result10 = Equation("=",Add(List(Div(List(Sub(List(Func("exp",ArgList(List(Mul(List(Num(2.0), Var("x")))))), Func("exp",ArgList(List(Neg(Var("x"))))))), Num(3.0))), Func("cosh",ArgList(List(Var("x")))))), Div(List(Add(List(Mul(List(Num(2.0), Func("exp",ArgList(List(Mul(List(Num(2.0), Var("x")))))))), Mul(List(Num(3.0), Func("exp",ArgList(List(Var("x")))))), Func("exp",ArgList(List(Neg(Var("x"))))))), Num(6.0))))

  val expr11 = "1-4x^2-4x^3"
  val result11 = Sub(List(Num(1.0), Mul(List(Num(4.0), Power(Var("x"),Num(2.0)))), Mul(List(Num(4.0), Power(Var("x"),Num(3.0))))))

  val expr12 = "sum(i=0..n, sum(j=0..i, a(j)*a(i-j) ))"
  val result12 = Iters("sum",Some(Equation("=",Var("i"), Num(0.0))),Some(Var("n")),Iters("sum",Some(Equation("=",Var("j"), Num(0.0))),Some(Var("i")),Mul(List(Func("a",ArgList(List(Var("j")))), Func("a",ArgList(List(Sub(List(Var("i"), Var("j"))))))))))

  val expr13 = "x(1+a^2) + x"
  val result13 = Add(List(Mul(List(Var("x"), Add(List(Num(1.0), Power(Var("a"),Num(2.0)))))), Var("x")))

  val expr14 = " Sum_{x=1..t} x^n * Product_{k=1..n} (k + x)/(1 + k*x)"
  val result14 = Iters("Sum",Some(Equation("=",Var("x"), Num(1.0))),Some(Var("t")),Mul(List(Power(Var("x"),Var("n")), Iters("Product",Some(Equation("=",Var("k"), Num(1.0))),Some(Var("n")),Div(List(Add(List(Var("k"), Var("x"))), Add(List(Num(1.0), Mul(List(Var("k"), Var("x")))))))))))

  val expr15 = "lim_{x->infty} 1/x"
  val result15 = Iters("lim",Some(Equation("->",Var("x"), Constant("infty"))),None,Div(List(Num(1.0), Var("x"))))

  val expr16 = "a(n) = Sum_{i=1..tau(n)} (tau(n)-i+1)*d_i "
  val result16 = Equation("=",Func("a",ArgList(List(Var("n")))), Iters("Sum",Some(Equation("=",Var("i"), Num(1.0))),Some(Func("tau",ArgList(List(Var("n"))))),Mul(List(Add(List(Sub(List(Func("tau",ArgList(List(Var("n")))), Var("i"))), Num(1.0))), Var("d_i")))))

  val expr17 = "1 + x/(1 + x/(1 + x/1 + x/(1 + x/(1 + ..."
  val result17 = Add(List(Num(1.0), Div(List(Var("x"), Add(List(Num(1.0), Div(List(Var("x"), Add(List(Num(1.0), Div(List(Var("x"), Num(1.0))), Div(List(Var("x"), Add(List(Num(1.0), Div(List(Var("x"), Add(List(Num(1.0), ExtraSymbol("...")))))))))))))))))))

  val expr18 = "a(n) = a(n-1) + 3a(n-2) + a(n-4)"
  val result18 = Equation("=",Func("a",ArgList(List(Var("n")))), Add(List(Func("a",ArgList(List(Sub(List(Var("n"), Num(1.0)))))), Mul(List(Num(3.0), Func("a",ArgList(List(Sub(List(Var("n"), Num(2.0)))))))), Func("a",ArgList(List(Sub(List(Var("n"), Num(4.0)))))))))

  val expr19 = "sum_(i=1..t^2+1; A213422(i) + T^2(i) + T(i^2))"
  val result19 = Iters("sum",Some(Equation("=",Var("i"), Num(1.0))),Some(Add(List(Power(Var("t"),Num(2.0)), Num(1.0)))),Add(List(FuncR(SeqReference("A213422"),ArgList(List(Var("i")))), Power(Func("T",ArgList(List(Var("i")))),Num(2.0)), Func("T",ArgList(List(Power(Var("i"),Num(2.0))))))))

  val expr20 = "sin(x^2^2 + 3^2) + log(T(x^2) + A123456(a(n+24) + a) + floor(x/2) + ceil(x^2/2)"
  val result20 = Add(List(Func("sin",ArgList(List(Add(List(Power(Var("x"),Power(Num(2.0),Num(2.0))), Power(Num(3.0),Num(2.0))))))), Func("log",ArgList(List(Add(List(Func("T",ArgList(List(Power(Var("x"),Num(2.0))))), FuncR(SeqReference("A123456"),ArgList(List(Add(List(Mul(List(Var("a"), Add(List(Var("n"), Num(24.0))))), Var("a")))))), Func("floor",ArgList(List(Div(List(Var("x"), Num(2.0)))))), Func("ceil",ArgList(List(Div(List(Power(Var("x"),Num(2.0)), Num(2.0)))))))))))))


  expr1 should "be parsed to" in {
    val parsed = FormulaParser.parse(expr1).get
    assert(parsed == result1)
  }

  expr2 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr2).get
    assert(parsed == result2)
  }

  expr3 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr3).get
    assert(parsed == result3)
  }

  expr4 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr4).get
    assert(parsed == result4)
  }

  expr5 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr5).get
    assert(parsed == result5)
  }

  expr6 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr6).get
    assert(parsed == result6)
  }

  expr7 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr7).get
    assert(parsed == result7)
  }

  expr8 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr8).get
    assert(parsed == result8)
  }

  expr9 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr9).get
    assert(parsed == result9)
  }

  expr10 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr10).get
    assert(parsed == result10)
  }

  expr11 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr11).get
    assert(parsed == result11)
  }

  expr12 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr12).get
    assert(parsed == result12)
  }

  expr13 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr13).get
    assert(parsed == result13)
  }

  expr14 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr14).get
    assert(parsed == result14)
  }

  expr15 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr15).get
    assert(parsed == result15)
  }

  expr16 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr16).get
    assert(parsed == result16)
  }

  expr17 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr17).get
    assert(parsed == result17)
  }

  expr18 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr18).get
    assert(parsed == result18)
  }

  expr19 should "be parsed to" in{
    val parsed = FormulaParser.parse(expr19).get
    assert(parsed == result19)
  }

}


