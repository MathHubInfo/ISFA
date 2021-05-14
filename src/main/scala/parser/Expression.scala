package parser


import java.io.Serializable

import org.json4s.ShortTypeHints
import org.json4s.native.Serialization
import parser.DocumentParser.GeneratingFunctionDefinition
import salat.annotations.Salat

import scala.xml.Elem

@Salat
sealed trait Expression{
  def present : String
  //  override def toString = present
  def toNode(implicit theory : String) : Elem
  def toSage: String
  def toSagePython: String = toSage
  def toCML: Elem
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
  def toCML = <ci>{name}</ci>
}
case class Num(double : Double) extends Expression{
  def present : String = if(double.isValidInt) double.toInt.toString else double.toString
  def toNode(implicit theory : String) : Elem = if(double.isValidInt) <OMI>{double.toInt}</OMI> else <OMF dec={double.toString}/>
  def toSage = present
  def toCML = <cn>{double}</cn>

  override def equals(o: Any) = o match {
    case that: Num => if(double == 0) that.double - double == 0 else Math.abs(1 - that.double / double) < 0.001
    case _ => false
  }
}
case class Constant(name : String) extends Expression{
  def present : String = name
  def toNode(implicit theory : String) : Elem = <OMS name={name}/> //TODO: FIGURE OUT THE RIGHT TAG
  def toSage = s"$name"
  def toCML = <ci>{name}</ci>
}

case class Abs(exp : Expression) extends Expression{
  def present : String = "|"+exp.present+"|"
  def toNode(implicit theory : String) = <OMS></OMS> // TODO:
  def toSage = s"|${exp.toSage}|"
  def toCML =
  <apply>
    <abs/>
    {exp.toCML}
  </apply>
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
  def toCML =
    <apply>
      <divide/>
      {num.toCML}
      {by.toCML}
    </apply>
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
  override def toSagePython = s"${base.toSagePython}**${exp.toSagePython}"
  def toCML =
  <apply>
    <power/>
    <ci>{base.toCML}</ci>
    <ci>{exp.toCML}</ci>
  </apply>

}
case class Add(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" + ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="plus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("+")})"
  override def toSagePython = s"(${expr.map(_.toSagePython).mkString("+")})"
  def toCML =
  <apply>
    <plus/>
    {expr.map(x =>
      <ci>{x.toCML}</ci>
    )}
  </apply>
}

case class Sub(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString(" - ")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="minus" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("-")})"
  override def toSagePython = s"(${expr.map(_.toSagePython).mkString("-")})"
  def toCML =
    <apply>
      <minus/>
      {expr.map(x =>
      <ci>{x.toCML}</ci>
    )}
    </apply>
}

case class Mul(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("*")
  def toNode(implicit theory : String) : Elem = <OMA><OMS name="times"/>{expr.map(_.toNode)}</OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("*")})"
  override def toSagePython = s"(${expr.map(_.toSagePython).mkString("*")})"
  def toCML =
    <apply>
      <times/>
      {expr.map(x =>
      <ci>{x.toCML}</ci>
    )}
    </apply>
}

case class Div(expr : List[Expression]) extends Expression{
  def present : String = expr.mkString("/")
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="divide" cd="arithmetics"/>
      {expr.map(_.toNode)}
    </OMA>
  def toSage = s"(${expr.map(_.toSage).mkString("/")})"
  override def toSagePython = s"(${expr.map(_.toSagePython).mkString("/")})"
  def toCML =
    <apply>
      <divide/>
      {expr.map(x =>
      <ci>{x.toCML}</ci>
    )}
    </apply>
}

case class Neg(expr : Expression) extends Expression{
  def present : String = s"(-${expr.toSage})"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="unary_minus"/>
      {expr.toNode}
    </OMA>
  def toSage = s"(-${expr.toSage})"
  override def toSagePython = s"(-${expr.toSagePython})"
  def toCML =
    <apply>
      <minus/>
      {expr.toCML}
    </apply>
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
  override def toSagePython = s"$name${args.toSagePython}"
  def toCML =
  <apply>
    <ci type="function">{name}</ci>
    {
      args.args.map(x =>
        <ci>{x.toCML}</ci>
      )
    }
  </apply>
}
case class FuncR(seq : SeqReference, args : ArgList) extends Expression{
  def present : String = seq.toString + args.toString
  def toNode(implicit theory : String) : Elem =
    <OMA>
      {seq.toNode}
      {args.args.map(_.toNode)}
    </OMA>
  def toSage = s"${seq.toSage}(${args.toSage})"
  override def toSagePython = s"${seq.toSagePython}(${args.toSagePython})"
  def toCML =
    <apply>
      {seq.toCML}
      {
      args.args.map(x =>
        <ci>{x.toCML}</ci>
      )
      }
    </apply>
}

case class ExtraSymbol(symbol : String) extends Expression{
  def present : String = symbol
  def toNode(implicit theory : String) : Elem = <OMS name={symbol} cd="arithmetics"></OMS>
  def toSage = symbol
  def toCML =
    <mo>{symbol}</mo>
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
  override def toSagePython = s"(${args.map(_.toSagePython).mkString(",")})"
  def toCML =
    <apply>
      <set>
      {
        args.map(_.toCML)
      }
      </set>
    </apply>
}
case class SeqReference(seq : String) extends Expression{
  def present : String = seq
  def toNode(implicit theory : String) : Elem = <OMR xref={seq}/>
  def toSage = seq
  def toCML = <ci>{seq}</ci>
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
  // {name} is one of the following: ['Product','product','Sum','sum','Prod','prod','Limit'] - as of the 2020 snapshot
  //this needs to look like this:
  /*
  <apply><sum/>
  <bvar><ci>x</ci></bvar>
  <lowlimit><ci>a</ci></lowlimit>
  <uplimit><ci>b</ci></uplimit>
  <apply><ci type="function">f</ci>
    <ci>x</ci>
  </apply>
</apply>

where <sum/> is the name?
   */
  def toCML =
    <apply>{
      if(name == "Sum" || name == "sum"){<sum/>}
      else{if(name == "Product" || name == "product" || name == "Prod" || name == "prod"){<product/>}
           else {if (name == "Limit" || name == "limit" ) {<limit/>}}
      }
      }
      <bvar><ci>x</ci></bvar>
      <lowlimit><ci>{from}</ci></lowlimit>
      <uplimit><ci>{to}</ci></uplimit>
      {on.toCML}
    </apply>
}
case class Factorial(expr : Expression) extends Expression{
  def present : String = expr.toString + "!"
  def toNode(implicit theory : String) : Elem =
    <OMA>
      <OMS name="factorial" cd="arithmetics"/>
      {expr.toNode}
    </OMA>
  def toSage = s"(${expr.toSage}!)"
  def toCML =
    <apply>
      <factorial/>
      {expr.toCML}
    </apply>
}

case class Equation(comparison : String, left : Expression, right : Expression) extends Expression{
  def present : String = left.toString + " = " + right.toString //wrong? but also unused
  def toNode(implicit theory : String) =
    <OMA>
      <OMS name={comparison} cd="arithmetic"/>
      {left.toNode}
      {right.toNode}
    </OMA>
  def toSage = s"${left.toSage} $comparison ${right.toSage}"
  override def toSagePython = s"${left.toSagePython} $comparison ${right.toSagePython}"
  /*
  * case diff and then one of these:
*           "=" --> 4.4.3.1 Equals <eq/>
            "!=" --> 4.4.3.2 Not Equals <neq/>
            ">" --> 4.4.3.3 Greater than <gt/>
            "<" --> 4.4.3.4 Less Than <lt/>
            ">=" -->4.4.3.5 Greater Than or Equal <geq/>
            "<=" --> 4.4.3.6 Less Than or Equal <leq/>
            4.4.3.7 Equivalent <equivalent/>
            4.4.3.8 Approximately <approx/>
            4.4.3.9 Factor Of <factorof/>
  * */

  def toCML =
    <apply>
      {if(comparison == "="){<eq/>}
       if(comparison == "!="){<neq/>}
       if(comparison == ">"){<gt/>}
       if(comparison == "<"){<lt/>}
       if(comparison == ">="){<geq/>}
       if(comparison == "<="){<leq/>}
       if(comparison == "<>"){<neq/>}
      }
      {left.toCML}
      {right.toCML}
    </apply>
}

case class Modulo(base : Expression, modulo : Expression) extends Expression {
  def present : String = base.toString + " mod " + modulo.toString
  def toNode(implicit theory : String) =
    Func("mod", ArgList(base::modulo::Nil)).toNode
  def toSage = s"${base.toSage} mod ${modulo.toSage}"
  // Remainder <rem/> -> this is modulo
  def toCML =
    <apply>
      <rem/>
      {base.toCML}
      {modulo.toCML}
    </apply>
}

case class InSet(element : Expression, set : Expression) extends Expression{
  def present : String = element.toString + " in " + set.toString
  def toNode(implicit theory : String) =
      <OMA>
        <OMBVAR>{ element.toNode }</OMBVAR>
        {set.toNode}
      </OMA>
  def toSage = s"${element.toSage} in ${set.toSage}"
  def toCML =
    <set>
      <bvar>{element.toCML}</bvar>
      <domainofapplication>{set.toCML}</domainofapplication>
    </set>
}


case class GeneratingFunction(expression: Expression) extends Expression{
  def present: String = "G.f" + expression.present

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = <OMA></OMA>

  def toSage = s"GF(x) = ${expression.toSage}"
  //ToDo this returns something of the form: " (of*A014405)" -> we don't want the gf part --> is <ci> the best solution?
  def toCML =
  <ci>
  {expression.toSage}
  </ci>
}

case class GeneratingFunctionDef(expression: ArgList) extends Expression{
  def present: String = "GF(" + expression.present + ")"

  //  override def toString = present
  override def toNode(implicit theory: String): Elem = <OMA></OMA>

  def toSage = s"GF(x) = ${expression.toSage}"
  def toCML = GeneratingFunction(expression.args.head).toCML
}

@Salat
trait Line extends Expression {
  def toSage = present
  def toCML = <c>NOT IMPLEMENTED</c>
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
  def toCML = <c>NOT IMPLEMENTED</c>
}