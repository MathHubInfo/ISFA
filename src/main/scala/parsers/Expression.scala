package parsers

import scala.xml.Elem

/**
 * Created by enxhi on 4/3/15.
 */
trait Expression{
  def present : String
  //  override def toString = present
  def toNode : Elem
  def clear : Expression
}

case class Var(name : String) extends Expression{
  def present : String = name
  def toNode : Elem = <OMV name={name}/>
  def clear = this
  override def toString = "Var("+ "\""+name+"\"" + ")"
}
case class Num(double : Double) extends Expression{
  def present : String = if(double.isValidInt) double.toInt.toString else double.toString
  def toNode : Elem = if(double.isValidInt) <OMI>{double.toInt}</OMI> else <OMF dec={double.toString}/>
  def clear = this
}
case class Constant(name : String) extends Expression{
  def present : String = name
  def toNode : Elem = <OMS name={name}/> //TODO: FIGURE OUT THE RIGHT TAG
  def clear = this
}


case class Power(base : Expression, exp : Expression) extends Expression{
  def present : String = base.toString +"^"+ exp.toString
  def toNode : Elem =
    <OMA>
      <OMS name="power"/>
      {base.toNode}
      {exp.toNode}
    </OMA>
  def clear = this
}
case class Add(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" + ")
  def toNode : Elem =
    <OMA>
      <OMS name="plus"/>
      {expr.map(_.toNode)}
    </OMA>
  def clear = this
}

case class Sub(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" - ")
  def toNode : Elem =
    <OMA>
      <OMS name="minus"/>
      {expr.map(_.toNode)}
    </OMA>
  def clear = this
}

case class Mul(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("*")
  def toNode : Elem = <OMA><OMS name="times"/>{expr.map(_.toNode)}</OMA>
  def clear : Expression = this
}

case class Div(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("/")
  def toNode : Elem = <OMA><OMS name="divide"/>{expr.map(_.toNode)}</OMA>
  def clear : Expression = this
}

case class Neg(expr : Expression) extends Expression{
  def present : String = ("-"+expr.toString)
  def toNode : Elem = <OMA><OMS name="unary_minus"/>{expr.toNode}</OMA>
  def clear : Expression = this
}
case class Func(name : String, args : ArgList) extends Expression{
  def present : String = name + args.toString
  def toNode : Elem = <OMA><OMS name={name}/>{args.args.map(_.toNode)}</OMA>
  def clear : Expression = this
}
case class FuncR(seq : SeqReference, args : ArgList) extends Expression{
  def present : String = seq.toString + args.toString
  def toNode : Elem = <OMA><OMS name={seq.toString}/>{args.args.map(_.toNode)}</OMA>
  def clear : Expression = this
}

case class ExtraSymbol(symbol : String) extends Expression{
  def present : String = symbol
  def toNode : Elem = <OMS name={symbol}></OMS>
  def clear : Expression = this
}
case class ArgList(args : List[Expression]) extends Expression{
  def present : String = "("+args.mkString(",")+")"
  def toNode : Elem = <OME>THIS SHOULD NEVER BE CALLED! WILL BE FIXED!</OME>
  def clear : Expression = this
}
case class SeqReference(seq : String) extends Expression{
  def present : String = seq
  def toNode : Elem = <OMR xref={seq}/>
  def clear : Expression = this
}

case class Iters(name : String, from : Option[Expression], to : Option[Expression], on : Expression) extends Expression{
  def present : String =
    name + "_{"+(if(from.isEmpty) "" else from.get.toString)+"}^{"+(if(to.isEmpty) "" else to.get.toString)+"}("+on.toString+")"
  def toNode : Elem = <p></p>
  def clear : Expression = this
}
case class Factorial(expr : Expression) extends Expression{
  def present : String = expr.toString + "!"
  def toNode : Elem =
    <OMA>
      <OMS name="factorial"/>
      {expr.toNode}
    </OMA>
  def clear : Expression = this
}

case class Equation(comparison : String, left : Expression, right : Expression) extends Expression{
  def present : String = left.toString + " = " + right.toString
  def toNode = <OMA>
    <OMS name={comparison}/>
    {left.toNode}
    {right.toNode}
  </OMA>
  def clear : Expression = this
}
//
case class Adder(expr : Expression) extends Expression{
  def present : String = expr.toString
  def toNode = <p></p>
  def clear : Expression = expr
}
case class Subber(expr : Expression) extends Expression{
  def present : String = expr.toString
  def toNode = <p></p>
  def clear : Expression = Neg(expr)
}
