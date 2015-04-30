package parser

import scala.xml.Elem

/**
 * Created by enxhi on 4/3/15.
 */
trait Expression{
  def present : String
  //  override def toString = present
  def toNode(implicit theory : String) : Elem
  def clear : Expression
}

case class Var(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMV name={name}/>
  def clear = this
  override def toString = "Var("+ "\""+name+"\"" + ")"
}
case class Num(double : Double) extends Expression{
  def present : String = if(double.isValidInt) double.toInt.toString else double.toString
  def toNode(implicit theory : String) : Elem = if(double.isValidInt) <OMI>{double.toInt}</OMI> else <OMF dec={double.toString}/>
  def clear = this
}
case class Constant(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMS name={name}/> //TODO: FIGURE OUT THE RIGHT TAG
  def clear = this
}


case class Power(base : Expression, exp : Expression) extends Expression{
  def present : String = base.toString +"^"+ exp.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="power" cd="arithmetics"/>
      {base.toNode}
      {exp.toNode}
    </OMA>
  def clear = this
}
case class Add(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" + ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="plus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def clear = this
}

case class Sub(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" - ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="minus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def clear = this
}

case class Mul(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("*")
  def toNode(implicit theory : String) : Elem = <OMA><OMS name="times"/>{expr.map(_.toNode)}</OMA>
  def clear : Expression = this
}

case class Div(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("/")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divide" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def clear : Expression = this
}

case class Neg(expr : Expression) extends Expression{
  def present : String = ("-"+expr.toString)
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="unary_minus"/>
      {expr.toNode}
    </OMA>
  def clear : Expression = this
}
case class Func(name : String, args : ArgList) extends Expression{
  def present : String = name + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name={name} cd={theory}/>
      {args.args.map(_.toNode)}
    </OMA>
  def clear : Expression = this
}
case class FuncR(seq : SeqReference, args : ArgList) extends Expression{
  def present : String = seq.toString + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      {seq.toNode}
      {args.args.map(_.toNode)}
    </OMA>
  def clear : Expression = this
}

case class ExtraSymbol(symbol : String) extends Expression{
  def present : String = symbol
  def toNode(implicit theory : String) : Elem = <OMS name={symbol} cd="arithmetics"></OMS>
  def clear : Expression = this
}
case class ArgList(args : List[Expression]) extends Expression{
  def present : String = "("+args.mkString(",")+")"
  def toNode(implicit theory : String) : Elem = <OME>THIS SHOULD NEVER BE CALLED! WILL BE FIXED!</OME>
  def clear : Expression = this
}
case class SeqReference(seq : String) extends Expression{
  def present : String = seq
  def toNode(implicit theory : String) : Elem = <OMR xref={seq}/>
  def clear : Expression = this
}

case class Iters(name : String, from : Option[Expression], to : Option[Expression], on : Expression) extends Expression{
  def present : String =
    name + "_{"+(if(from.isEmpty) "" else from.get.toString)+"}^{"+(if(to.isEmpty) "" else to.get.toString)+"}("+on.toString+")"
  def toNode(implicit theory : String) : Elem =
    <OMS name="name">
      <OMBIND>
          <OMA>
            <OMS name={name} cd="arithmetics"/>
              <OMA>
                <OMS name="interval">
                  {if(!from.isEmpty) {
                      from.get.toNode
                    }
                  }
                  {if(!to.isEmpty) {
                      to.get.toNode
                    }
                  }
                </OMS>
              </OMA>
              <OMBVAR>
                {if(!from.isEmpty) {
                    from.get match {
                      case Equation(eq, Var(a), rest) => Var(a).toNode
                      case _ => ""
                    }
                  }
                }
              </OMBVAR>
            {on.toNode}
          </OMA>
      </OMBIND>
    </OMS>
  def clear : Expression = this
}
case class Factorial(expr : Expression) extends Expression{
  def present : String = expr.toString + "!"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="factorial" cd="arithmetics"/>
      {expr.toNode}
    </OMA>
  def clear : Expression = this
}

case class Equation(comparison : String, left : Expression, right : Expression) extends Expression{
  def present : String = left.toString + " = " + right.toString
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name={comparison} cd="arithmetic"/>
      {left.toNode}
      {right.toNode}
    </OMA>
  def clear : Expression = this
}
//
case class Adder(expr : Expression) extends Expression{
  def present : String = expr.toString
  def toNode(implicit theory : String) = <p></p>
  def clear : Expression = expr
}
case class Subber(expr : Expression) extends Expression{
  def present : String = expr.toString
  def toNode(implicit theory : String) = <p></p>
  def clear : Expression = Neg(expr)
}
