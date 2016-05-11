package parser


import java.io.Serializable

import com.novus.salat.annotations.Salat
import org.json4s.{ShortTypeHints, NoTypeHints}
import org.json4s.native.Serialization
import parser.DocumentParser.GeneratingFunctionDefinition
import play.api.libs.json.{JsResult, JsValue, Format, Json}

import scala.xml.Elem

/**
 * Created by enxhi on 4/3/15.
 */

@Salat
sealed trait Expression{
  def present : String
  //  override def toString = present
  def toNode(implicit theory : String) : Elem
  def toSage: String
}

object Expression {
  val typeHints = ShortTypeHints(List(
    classOf[Var],
    classOf[GeneratingFunction],
    classOf[Num],
    classOf[Constant],
    classOf[Abs],
    classOf[Divisible],
    classOf[Power],
    classOf[Add],
    classOf[Sub],
    classOf[Mul],
    classOf[Div],
    classOf[Neg],
    classOf[Func],
    classOf[FuncR],
    classOf[ExtraSymbol],
    classOf[ArgList],
    classOf[SeqReference],
    classOf[Iters],
    classOf[Factorial],
    classOf[Equation],
    classOf[Modulo],
    classOf[InSet],
    classOf[KSet],
    classOf[GeneratingFunctionDef],
    classOf[Sentence],
    classOf[Delim],
    classOf[Word],
    classOf[Name],
    classOf[Date],
    classOf[Email],
    classOf[GeneratingFunctionDefinition],
    classOf[QVar],
    classOf[List[Expression]]
  ))
  implicit val format = Serialization.formats(typeHints)
}

case class Var(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMV name={name}/>
  override def toString = "Var("+ "\""+name+"\"" + ")"
  def toSage = s"$name"
}
case class Num(double : Double) extends Expression{
  def present : String = if(double.isValidInt) double.toInt.toString else double.toString
  def toNode(implicit theory : String) : Elem = if(double.isValidInt) <OMI>{double.toInt}</OMI> else <OMF dec={double.toString}/>
  def toSage = present
}
case class Constant(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMS name={name}/> //TODO: FIGURE OUT THE RIGHT TAG
  def toSage = s"$name"
}

case class Abs(exp : Expression) extends Expression{
  def present : String = "|"+exp.present+"|"
  def toNode(implicit theory : String) = <OMS></OMS> // TODO:
  def toSage = s"|${exp.toSage}|"
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
  def toSage = s"(${num.toSage}|${by.toSage})"
}

case class Power(base : Expression, exp : Expression) extends Expression{
  def present : String = base.toString +"^"+ exp.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="power" cd="arithmetics"/>
      {base.toNode}
      {exp.toNode}
    </OMA>
  def toSage = s"${base.toSage}^${exp.toSage}"
}
case class Add(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" + ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="plus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("+")})"
}

case class Sub(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" - ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="minus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("-")})"
}

case class Mul(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("*")
  def toNode(implicit theory : String) : Elem = <OMA><OMS name="times"/>{expr.map(_.toNode)}</OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("*")})"
}

case class Div(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("/")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divide" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("/")})"
}

case class Neg(expr : Expression) extends Expression{
  def present : String = ("-"+expr.toString)
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="unary_minus"/>
      {expr.toNode}
    </OMA>
  def toSage = s"(-${expr.toSage})"
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
  def toSage = s"$name${args.toSage}"
}
case class FuncR(seq : SeqReference, args : ArgList) extends Expression{
  def present : String = seq.toString + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      {seq.toNode}
      {args.args.map(_.toNode)}
    </OMA>
  def toSage = s"${seq.toSage}${args.toSage}}"
}

case class ExtraSymbol(symbol : String) extends Expression{
  def present : String = symbol
  def toNode(implicit theory : String) : Elem = <OMS name={symbol} cd="arithmetics"></OMS>
  def toSage = symbol
}

case class ArgList(args : List[Expression]) extends Expression{
  def present : String = "("+args.mkString(",")+")"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS cd="set1" name="set">
        {args.map(_.toNode)}
      </OMS>
    </OMA>
  def toSage = s"(${args.map(_.toSage).mkString(",")})"
}
case class SeqReference(seq : String) extends Expression{
  def present : String = seq
  def toNode(implicit theory : String) : Elem = <OMR xref={seq}/>
  def toSage = seq
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

  def toSage = present
}
case class Factorial(expr : Expression) extends Expression{
  def present : String = expr.toString + "!"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="factorial" cd="arithmetics"/>
      {expr.toNode}
    </OMA>
  def toSage = s"(${expr.toSage}!)"
}

case class Equation(comparison : String, left : Expression, right : Expression) extends Expression{
  def present : String = left.toString + " = " + right.toString
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name={comparison} cd="arithmetic"/>
      {left.toNode}
      {right.toNode}
    </OMA>
  def toSage = s"${left.toSage} $comparison ${right.toSage}"
}

case class Modulo(base : Expression, modulo : Expression) extends Expression {
  def present : String = base.toString + " mod " + modulo.toString
  def toNode(implicit theory : String) =
    Func("mod", ArgList(base::modulo::Nil)).toNode
  def toSage = s"${base.toSage} mod ${modulo.toSage}"
}

case class InSet(element : Expression, set : Expression) extends Expression{
  def present : String = element.toString + " in " + set.toString
  def toNode(implicit theory : String) =
      <OMA>
        <OMBVAR>{ element.toNode }</OMBVAR>
        {set.toNode}
      </OMA>
  def toSage = s"${element.toSage} in ${set.toSage}"
}

case class KSet(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name="in" cd="arithmetic" />
      <OMS name={name} cd="arithmetic"/>
    </OMA>
  def toSage = present
}

case class GeneratingFunction(expression: Expression) extends Expression{
  def present: String = "G.f" + expression.present

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = <OMA></OMA>

  def toSage = s"GF(x) = ${expression.toSage}"
}

case class GeneratingFunctionDef(expression: ArgList) extends Expression{
  def present: String = "GF(" + expression.present + ")"

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = <OMA></OMA>

  def toSage = s"GF(x) = ${expression.toSage}"
}

@Salat
trait Line extends Expression {
  def toSage = present
}

case class Sentence(parts : List[Expression]) extends Line{
  override def present: String = parts.mkString("")

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = {
    <CMP>
      {parts.map {
      case a: Line => a.present
      case a: Expression => <OMOBJ>
        {a.toNode}
      </OMOBJ>
    }}
    </CMP>
  }

  def toSubNode(implicit theory: String): List[Serializable] = {
    parts.map {
      case a: Sentence => a.toSubNode
      case a: Line => a.present
      case a: Expression => <OMOBJ>{ a.toNode }</OMOBJ>
    }
  }
}

case class Delim(delim : String) extends Line{
  override def present: String = delim

  //  override def toString = present
  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{delim}</text>
}

case class Word(word : String) extends Line{
  override def present: String = word+" "

  //  override def toString = present

  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{word}</text>
}

case class Name(name : String) extends Line{
  override def present: String = name+" "

  //  override def toString = present
  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{name}</text>
}

case class Date(date : String) extends Line{
  override def present: String = date+" "

  //  override def toString = present
  //shouldn't be called
  override def toNode(implicit theory: String): Elem = <text>{date}</text>
}

case class Email(email : String) extends Line{
  override def present: String = email+" "

  override def toNode(implicit theory: String): Elem = <text>{email}</text>
}


case class QVar(expr : Expression) extends Expression{
  def present : String = expr.present
  def toNode(implicit theory : String) : Elem = <QVAR>{expr.toNode}</QVAR>
  def clear = this
  override def toString = "QVar(" + expr.toString +")"
  def toSage = s"${expr.toSage}"
}