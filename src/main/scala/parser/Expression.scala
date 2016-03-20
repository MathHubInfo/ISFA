package parser

import scala.xml.Elem

/**
 * Created by enxhi on 4/3/15.
 */
trait Expression{
  def present : String
  //  override def toString = present
  def toNode(implicit theory : String) : Elem
}

case class Var(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMV name={name}/>
  override def toString = "Var("+ "\""+name+"\"" + ")"
}
case class Num(double : Double) extends Expression{
  def present : String = if(double.isValidInt) double.toInt.toString else double.toString
  def toNode(implicit theory : String) : Elem = if(double.isValidInt) <OMI>{double.toInt}</OMI> else <OMF dec={double.toString}/>
}
case class Constant(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMS name={name}/> //TODO: FIGURE OUT THE RIGHT TAG
}

case class Abs(exp : Expression) extends Expression{
  def present : String = "|"+exp.present+"|"
  def toNode(implicit theory : String) = <OMS></OMS> // TODO:
}

case class Divisible(num : Expression, by : Expression) extends Expression{
  def present : String = num + "|" + by
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divisible" cd="arithemtics">
        {num.toNode}
        {by.toNode}
      </OMS>
    </OMA>
}

case class Power(base : Expression, exp : Expression) extends Expression{
  def present : String = base.toString +"^"+ exp.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="power" cd="arithmetics"/>
      {base.toNode}
      {exp.toNode}
    </OMA>
}
case class Add(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" + ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="plus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
}

case class Sub(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" - ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="minus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
}

case class Mul(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("*")
  def toNode(implicit theory : String) : Elem = <OMA><OMS name="times"/>{expr.map(_.toNode)}</OMA>
}

case class Div(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("/")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divide" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
}

case class Neg(expr : Expression) extends Expression{
  def present : String = ("-"+expr.toString)
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="unary_minus"/>
      {expr.toNode}
    </OMA>
}
case class Func(name : String, args : ArgList) extends Expression{
  def present : String = name + args.toString
  def toNode(implicit theory : String) : Elem =
    if(args.args.nonEmpty) {
      <OMA>
        <OMS name={name} cd={theory}/>{args.args.map(_.toNode)}
      </OMA>
    }else{
      <OMS name={name} cd="arithmetic"/>
    } //TODO : the last one is "function()" like phi(), fix it
}
case class FuncR(seq : SeqReference, args : ArgList) extends Expression{
  def present : String = seq.toString + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      {seq.toNode}
      {args.args.map(_.toNode)}
    </OMA>
}

case class ExtraSymbol(symbol : String) extends Expression{
  def present : String = symbol
  def toNode(implicit theory : String) : Elem = <OMS name={symbol} cd="arithmetics"></OMS>
}
case class ArgList(args : List[Expression]) extends Expression{
  def present : String = "("+args.mkString(",")+")"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS cd="set1" name="set">
        {args.map(_.toNode)}
      </OMS>
    </OMA>
}
case class SeqReference(seq : String) extends Expression{
  def present : String = seq
  def toNode(implicit theory : String) : Elem = <OMR xref={seq}/>
}

case class Iters(name : String, from : Option[Expression], to : Option[Expression], on : Expression) extends Expression{
  def present : String =
    name + "_{"+(if(from.isEmpty) "" else from.get.toString)+"}^{"+(if(to.isEmpty) "" else to.get.toString)+"}("+on.toString+")"
  def toNode(implicit theory : String) : Elem = {
      val regular =
        <OMBIND>
          <OMA>
            <OMS name={name} cd="arithmetics"/>
            <OMA>
              <OMS name="interval" cd="arithmetics"/>
              {if(from.nonEmpty) {
              from.get.toNode
            }
              }
              {if(to.nonEmpty) {
              to.get.toNode
            }
              }
            </OMA>
          </OMA>
          {if(from.nonEmpty) {
          from.get match {
            case Equation(eq, Var(a), rest) =>
              <OMBVAR>{ Var(a).toNode }</OMBVAR>
            case _ => ""
          }
        }
          }
          <OMA>
            {on.toNode}
          </OMA>
        </OMBIND>
      if(from.nonEmpty){
        from.get match {
          case exp : InSet =>
            <OMBIND>
              <OMA>
                <OMS name={name} cd="arithmetics"/>
              </OMA>
              {
              exp match {
                case InSet(variable, set) =>
                  <OMBVAR>{ variable }</OMBVAR>
                  set.toNode
               }
              }
              <OMA>
                {on.toNode}
              </OMA>
            </OMBIND>
          case _ => regular
        }
      }else{
        regular
      }
  }
}
case class Factorial(expr : Expression) extends Expression{
  def present : String = expr.toString + "!"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="factorial" cd="arithmetics"/>
      {expr.toNode}
    </OMA>
}

case class Equation(comparison : String, left : Expression, right : Expression) extends Expression{
  def present : String = left.toString + " = " + right.toString
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name={comparison} cd="arithmetic"/>
      {left.toNode}
      {right.toNode}
    </OMA>
}

case class Modulo(base : Expression, modulo : Expression) extends Expression {
  def present : String = base.toString + " mod " + modulo.toString
  def toNode(implicit theory : String) =
    Func("mod", ArgList(base::modulo::Nil)).toNode
}

case class InSet(element : Expression, set : Expression) extends Expression{
  def present : String = element.toString + " in " + set.toString
  def toNode(implicit theory : String) =
      <OMA>
        <OMBVAR>{ element.toNode }</OMBVAR>
        {set.toNode}
      </OMA>
}

case class KSet(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name="in" cd="arithmetic" />
      <OMS name={name} cd="arithmetic"/>
    </OMA>
}

case class GeneratingFunction(expression: Expression) extends Expression{
  def present: String = "G.f" + expression.present

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = <OMA></OMA>
}

case class GeneratingFunctionDef(expression: Expression) extends Expression{
  def present: String = "GF(" + expression.present + ")"

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = <OMA></OMA>
}
