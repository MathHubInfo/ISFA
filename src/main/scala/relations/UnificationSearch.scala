package relations

import java.util
import java.util.concurrent.ConcurrentHashMap

import parser._
import Expression.format
import com.sun.org.apache.xpath.internal.operations.Variable
import org.json4s.{ShortTypeHints, FullTypeHints, NoTypeHints}
import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import org.slf4j.LoggerFactory
import parser.DocumentParser.GeneratingFunctionDefinition
import parser._
import play.api.Logger
import play.api.libs.json.Json
import sage.SageWrapper

import scala.collection.immutable.{::, HashMap}
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object UnificationSearch {
  def unify(formula: Expression, withFormula: Expression) = {

  }
}

object GeneratingFunctionSearch {
  def logger = LoggerFactory.getLogger(this.getClass)
  private val renameBase = "vari"
  val hashMap = new ConcurrentHashMap[Expression, (List[(String, String, String, String)])].asScala
  var validGFS = 0

  case class ParsedSageTheory(generatingFunctionPartial: Expression, extractedPartials: List[List[PartialFractionAndTransform]])

  def parseSageTheory(sage: String): ParsedSageTheory = {
    val parsed = read[PartialFractionToTransforms](sage)
    ParsedSageTheory(read[Expression](parsed.expression), parsed.transforms.map(_.map(read[PartialFractionAndTransform])))
  }

  def getGeneratingFunction(expression: GeneratingFunctionDefinition): Expression = {
    getGeneratingFunction(expression.body)
  }

  def getGeneratingFunction(expression: Expression): Expression = expression match {
    case Equation(_, left, right) => getGeneratingFunction(right)
    case x => x
  }

  def getSeqReference(e: Expression): List[String] = e match {
    case Sentence(parts) => parts.flatMap(getSeqReference)
    case Var(name) => Nil
    case Abs(expr) => getSeqReference(expr)
    case Divisible(left, right) => getSeqReference(left) ::: getSeqReference(right)
    case Power(base, expr) => getSeqReference(base) ::: getSeqReference(expr)
    case Add(expressions) => expressions.flatMap(getSeqReference)
    case Sub(expressions) => expressions.flatMap(getSeqReference)
    case Mul(expressions) => expressions.flatMap(getSeqReference)
    case Div(expressions) => expressions.flatMap(getSeqReference)
    case Neg(expressions) => getSeqReference(expressions)
    case Func(name, args) => getSeqReference(args)
    case ArgList(args) => args.flatMap(getSeqReference)
    case Iters(name, from, to , on) =>
      getSeqReference(on) ::: from.map(getSeqReference).toList.flatten ::: to.map(getSeqReference).toList.flatten
    case Factorial(expr) => getSeqReference(expr)
    case Equation(_, left, right) =>
      getSeqReference(left) ::: getSeqReference(right)
    case Modulo(base, modulo) => getSeqReference(base) ::: getSeqReference(modulo)
    case GeneratingFunction(expressions) => getSeqReference(expressions)
    case GeneratingFunctionDef(expressions) => getSeqReference(expressions)
    case SeqReference(name) => name :: Nil
    case x => List()
  }

  def rename(ex: Expression): Expression = {
    val currentRenaming = collection.mutable.Map[String,String]()
    val stack: collection.mutable.ListBuffer[Unit => Expression] = ListBuffer.empty

    def renameVariables(e: Expression): Expression = e match {
      case Var(name) if currentRenaming.isDefinedAt(name) => Var(currentRenaming(name))
      case Var(name) =>
        val newName = renameBase + currentRenaming.size
        currentRenaming.put(name, newName)
        Var(newName)
      case Abs(expr) => Abs(renameVariables(expr))
      case Divisible(left, right) => Divisible(renameVariables(left), renameVariables(right))
      case Power(base, expr) => Power(renameVariables(base), renameVariables(expr))
      case Add(expressions) => Add(expressions.map(renameVariables))
      case Sub(expressions) => Sub(expressions.map(renameVariables))
      case Mul(expressions) => Mul(expressions.map(renameVariables))
      case Div(expressions) => Div(expressions.map(renameVariables))
      case Neg(expressions) => Neg(renameVariables(expressions))
      case Func(name, args) => Func(name, renameVariables(args).asInstanceOf[ArgList])
      case ArgList(args) => ArgList(args.map(renameVariables))
      case Iters(name, from, to , on) => Iters(name,
        from.map(renameVariables),
        to.map(renameVariables),
        renameVariables(on))
      case Factorial(expr) => Factorial(renameVariables(expr))
      case Equation(comparions, left, right) =>
        Equation(comparions, renameVariables(left), renameVariables(right))
      case Modulo(base, modulo) => Modulo(renameVariables(base), renameVariables(modulo))
      case GeneratingFunction(expressions) => GeneratingFunction(renameVariables(expressions))
      case GeneratingFunctionDef(expressions) => GeneratingFunctionDef(renameVariables(expressions).asInstanceOf[ArgList])
      case x => x
    }

    renameVariables(ex)
  }

  def getVariables(expres: Expression): List[String] = {
    def getVariablesHelper(e: Expression): List[String] = e match {
      case Var(name) => name :: Nil
      case Abs(expr) => getVariablesHelper(expr)
      case Divisible(left, right) => getVariablesHelper(left) ::: getVariablesHelper(right)
      case Power(base, expr) => getVariablesHelper(base) ::: getVariablesHelper(expr)
      case Add(expressions) => expressions.flatMap(getVariablesHelper)
      case Sub(expressions) => expressions.flatMap(getVariablesHelper)
      case Mul(expressions) => expressions.flatMap(getVariablesHelper)
      case Div(expressions) => expressions.flatMap(getVariablesHelper)
      case Neg(expressions) => getVariablesHelper(expressions)
      case Func(name, args) => getVariablesHelper(args)
      case ArgList(args) => args.flatMap(getVariablesHelper)
      case Iters(name, from, to , on) =>
       getVariablesHelper(on) ::: from.map(getVariablesHelper).toList.flatten ::: to.map(getVariablesHelper).toList.flatten
      case Factorial(expr) => getVariablesHelper(expr)
      case Equation(_, left, right) =>
        getVariablesHelper(left) ::: getVariablesHelper(right)
      case Modulo(base, modulo) => getVariablesHelper(base) ::: getVariablesHelper(modulo)
      case GeneratingFunction(expressions) => getVariablesHelper(expressions)
      case GeneratingFunctionDef(expressions) => getVariablesHelper(expressions)
      case x => Nil
    }

    getVariablesHelper(expres)
  }

  def extractPartialFractionSummands(partialFractions: Expression): List[Expression] = partialFractions match {
    case Add(expr) => expr.flatMap(extractPartialFractionSummands)
    case Sub(expr) => expr.flatMap(extractPartialFractionSummands)
    case x => x :: Nil
  }

  def makeTransformations(expression: Expression): List[(Expression,String)] = {
    val integrate = SageWrapper.integrate(expression)
    val differentiate = SageWrapper.derivative(expression)
    val same = expression

    (same, "unit") :: integrate.toList.map((_, "integrate")) ::: differentiate.toList.map((_, "differentiate"))
  }

  def checkIfExists(expression: Expression, id: String) = {
    val partialFractionsOpt = SageWrapper.partialFraction(expression)
    partialFractionsOpt.foreach { partialFractions =>
      validGFS += 1
      val extractedGFs = extractPartialFractionSummands(partialFractions)
      val transformed = extractedGFs.flatMap(makeTransformations)
      transformed.foreach { gf =>
        val renamed = rename(removeXMultiplications(removeConstants(gf._1)))
        if(hashMap.contains(renamed)) {
          Logger.debug(s"Partial fraction summand transformed already there ${renamed.toSage} with $id from \n ${expression.toSage} \n " +
            s"from partial fraction \n ${partialFractions.toSage} \n")
          hashMap.put(renamed, (id, gf._2, expression.toSage, partialFractions.toSage) :: hashMap(renamed))
        }
        else hashMap.put(renamed, (id, gf._2, expression.toSage, partialFractions.toSage) :: Nil)
      }
    }
  }

  def equal(left: Expression, right: Expression) = {
//    println(s"checking equality of $left and $right")

    rename(left) == rename(right)
  }

  def removeNegation(expression: Expression): Expression = expression match {
    case Neg(a) => removeNegation(a)
    case Mul(list) => Mul(list.map(removeNegation))
    case Div(list) => Div(list.map(removeNegation))
    case Sub(list) => Sub(list.map(removeNegation))
    case Add(list) => Add(list.map(removeNegation))
    case x => x
  }

  def removeConstants(expression: Expression): Expression = removeNegation(expression) match {
    case Mul(Num(a)::b::Nil) => b
    case Mul(Num(a)::b) => removeConstants(Mul(b))
    case Mul(Div(Num(a) :: Num(b)::Nil)::c::nil) => removeConstants(c)
    case Div(Num(a)::Num(b)::c::Nil) => Div(Num(1)::c::Nil)
    case Div(Num(a)::c::Nil) => Div(Num(1)::c::Nil)
    case Div(Mul(Num(a)::b::Nil)::c::Nil) => Div(b::c::Nil)
//    case Div(list) if list.length > 3 => throw new Exception(s"Cannot deal with ${list}")
    case x => x
  }

  def removeConstantsWithFeedback(expression: Expression): (Expression, Double) = removeNegation(expression) match {
    case Mul(Num(a)::b::Nil) => (b, a)
    case Mul(Num(a)::b) =>
      val result = removeConstantsWithFeedback(Mul(b))
      (result._1, result._2 * a)
    case Mul(Div(Num(a) :: Num(b)::Nil)::c::nil) =>
      val result = removeConstantsWithFeedback(c)
      (result._1, result._2 * (a / b))
    case Div(Num(a)::Num(b)::c::Nil) => (Div(Num(1)::c::Nil), a/b)
    case Div(Num(a)::c::Nil) => (Div(Num(1)::c::Nil), a)
    case Div(Mul(Num(a)::b::Nil)::c::Nil) => (Div(b::c::Nil), a)
    //    case Div(list) if list.length > 3 => throw new Exception(s"Cannot deal with ${list}")
    case x => (x, 1)
  }

  def removeXMultiplications(expression: Expression): Expression = removeNegation(expression) match {
    case Div(Power(Var("x"), exp)::b::Nil) => Div(Num(1)::b::Nil)
    case Div(Var("x")::exp::Nil) => Div(Num(1)::exp::Nil)
    case x => x
  }

  def removeXMultiplicationsWithFeedback(expression: Expression): (Expression, Double) = removeNegation(expression) match {
    case Div(Power(Var("x"), Num(exp))::b::Nil) => Div(Num(1)::b::Nil) -> exp
    case Div(Var("x")::exp::Nil) => Div(Num(1)::exp::Nil) -> 1
    case x => x -> 0
  }

  def normalizeFractions(expression: Expression): Expression = expression match {
    case Div(Num(constant)::Power(Sub(Num(1) :: Mul(bx)::Nil), Num(k)) :: Nil) =>
      Div(Num(1) :: Sub(Num(1) :: removeConstants(Mul(bx)) :: Nil) :: Nil)
    case Div(Num(constant)::Sub(Num(1) :: Mul(bx) :: Nil) :: Nil) =>
      Div(Num(1) :: Sub(Num(1) ::removeConstants(Mul(bx)) :: Nil) :: Nil)
    case x => x
  }

  def tryFunctionMatch() = {
    val theories = DocumentDao.findAll().toArray
    println(s"Length is ${theories.length}")

    import parser.Expression._

    for (i <- theories.indices) {
      val mainTheory = theories(i)
      println(i)
      mainTheory.pureGeneratingFunctions.foreach { mainGF =>
        val mainParsedGF = read[Expression](mainGF)
        val expression = mainParsedGF
        val id = mainTheory.theory
        val parsedSageTheories = mainTheory.sage.map(parseSageTheory)

        parsedSageTheories.foreach { parsedSageTheory =>
          val transformed = parsedSageTheory.extractedPartials.flatten
          transformed.foreach { gf =>
            val renamed = rename(gf.expression)
            if (hashMap.contains(renamed)) {
//              Logger.debug(s"Partial fraction summand transformed already there ${renamed.toSage} with $id from \n ${expression.toSage} \n " +
//                s"from partial fraction \n ${gf.expression.toSage} \n")
              hashMap.put(renamed, (id, gf.transform, expression.toSage, gf.expression.toSage) :: hashMap(renamed))
            }
            else hashMap.put(renamed, (id, gf.transform, expression.toSage, gf.expression.toSage) :: Nil)
          }
        }

      }
    }

    val filtered = hashMap.filter { case (k, v) => {
      k match {
        case Num(f) => false
        case Var(_) => false
        case Div(List(Num(_), Num(_))) => false
        case Power(Var(x), p) => false
        case Div(List(Num(const), Sub(List(Var(x), Num(1))))) => false
        case Func("log", ArgList(List(Sub(Num(1) :: Var(x) :: Nil)))) => false
        case _ => v.map(_._1).distinct.length > 1
      }
    }
    }

    hashMap.empty

//    filtered.foreach { case (k, v) => if (v.length > 1) logger.debug(s"Intersection $k \n ${k.toSage} with ${v.map(_._1.distinct)} \n") }
    logger.debug("The number of matches " + filtered.count { case (k, v) => v.length > 1 })

    val graph = new collection.mutable.HashMap[String, List[String]]()
//    val graph = new collection.mutable.HashMap[String, Int]()
    val sum = 0
    filtered.foreach { case (k, v) =>
      val theories = v.map(_._1).distinct
      theories.foreach { theory =>
        val connections = theories.filterNot(_ == theory).distinct
        if (graph.contains(theory)) {
          graph.put(theory, (connections ::: graph(theory)).distinct)
//          graph.put(theory, graph(theory) + connections.length)
        } else {
          graph.put(theory, connections)
//          graph.put(theory, connections.length)
        }
      }
    }
    filtered.empty

    filtered.foreach { case (k, v) => if (v.length > 1) {
//      logger.debug(s"Intersection ${k.toSage}")
    }
    }
    logger.debug(s"Number of relations found: ${graph.map(_._2.length).sum / 2}")
    logger.debug(s"Number of intersection partials: ${filtered.count(_._2.length > 1)}")
    logger.debug(s"Number of unique partials: ${filtered.count(_._2.length == 1)}")
    logger.debug(s"The unique partials:\n ${filtered.filter(_._2.length == 1).map(_._1.toSage).mkString("\n")}")

//    val script = graph.toList.sortBy(_._1.substring(1).toInt).map { case (k, v) =>
//      s"""{ "name": "$k", "size": 10, "imports": [${v.map(x => "\"" + x + "\"").mkString(",")}] } """
//    }.mkString(",")
    //
//    logger.debug("script being printed")
//    logger.debug(script)
  }

  def populateDatabase() = {
    val theories = DocumentDao.findAll().toArray
    theories.foreach { theory =>
      logger.debug(s"Theory: ${theory.theory}")
      val generatingFunctions = theory.pureGeneratingFunctions.map(read[Expression])
      val m = generatingFunctions.map { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction)
        partialFractionsOpt.map { partialFractions =>
          val partialFractionList = extractPartialFractionSummands(partialFractions)
          PartialFractionToTransforms(write[Expression](partialFractions), partialFractionList.map(x => makeTransformations(x).map(x => PartialFractionAndTransform(x._1, x._2)).map { x =>
            val serialized = write[PartialFractionAndTransform](x)
//            println(serialized)
            serialized
          }))
        }
      }.filter(_.isDefined).map(_.get).toList

      theory.sage = m.map(write[PartialFractionToTransforms](_))
      DocumentDao.updateDao(theory)
    }
  }

  def populateUnified() = {
    val theories = DocumentDao.findAll().toArray
    println(s"Length is ${theories.length}")

    import parser.Expression._

    val indices = theories.indices

    for(i <- indices){
      val mainTheory = theories(i)
      println(i)
      val parsedSageTheories = mainTheory.sage.map(parseSageTheory)
      mainTheory.sageUnified = parsedSageTheories.map { parsedSageTheory =>
        val transforms = parsedSageTheory.extractedPartials.map { partialFractionAndTransformList =>
          partialFractionAndTransformList.map { partialFractionAndTransform =>
            val unified = removeXMultiplications(removeConstants(partialFractionAndTransform.expression))
            SageWrapper.partialFraction(unified) -> partialFractionAndTransform.transform
          }.filter(_._1.isDefined).map(x => write(PartialFractionAndTransform(x._1.get, x._2)))
        }
        PartialFractionToTransforms(write(parsedSageTheory.generatingFunctionPartial), transforms)
      }.map(write[PartialFractionToTransforms])

      DocumentDao.updateDao(mainTheory)
    }
  }

  def populatePartialized() = {
    val theories = DocumentDao.findAll().toArray
    println(s"Length is ${theories.length}")

    import parser.Expression._

    val indices = theories.indices

    for(i <- indices){
      val mainTheory = theories(i)
      println(i)
      mainTheory.pureGeneratingFunctionsPartialized = mainTheory.pureGeneratingFunctions.flatMap { mainGF =>
        val mainParsedGF = read[Expression](mainGF)

        SageWrapper.partialFraction(removeXMultiplications(removeConstants(mainParsedGF))).toList
      }.map(write[Expression]).toList
      DocumentDao.updateDao(mainTheory)
    }
  }

  def tryFunctionMatchWithDirectSubs() = {
    val hashSet = new ConcurrentHashMap[Expression, List[String]]().asScala
    val theories = DocumentDao.findAll().toArray
    println(s"Length is ${theories.length}")

    import parser.Expression._

    val indices = theories.indices

    for(i <- indices){
      val mainTheory = theories(i)
      println(i)
      mainTheory.pureGeneratingFunctionsPartialized.foreach { mainGF =>
        val gfProcessed = read[Expression](mainGF)

        val value = rename(removeXMultiplications(removeConstants(gfProcessed)))
        if(hashSet.contains(value)) {
          hashSet.put(value, mainTheory.theory::hashSet(value))
        } else hashSet.put(value, List(mainTheory.theory))
      }
    }

    val map = new mutable.HashMap[String, mutable.Map[Expression, List[(Expression, String, List[String])]]]()
    for(
      i <- indices
    ) {
        val expressions = theories(i).pureGeneratingFunctions
        val theoryId = theories(i).theory
        val partialFractionHashMap = new mutable.HashMap[Expression, List[(Expression, String, List[String])]]()
        map.put(theoryId, partialFractionHashMap)

        val parsedSageTheories = theories(i).sageUnified.map(parseSageTheory)

        parsedSageTheories.map { parsedSageTheory  =>
          val expression = parsedSageTheory.generatingFunctionPartial
//        println(s"Expression: ${expression.toSage}")

          val extractedGFs = parsedSageTheory.extractedPartials.filter(_.nonEmpty)

          extractedGFs.map { extractedPartialAndTransformations =>
            println(extractedPartialAndTransformations)
            val partialFraction = extractedPartialAndTransformations.head.expression
            map(theoryId).put(partialFraction, Nil)

//            println(s"Partial fraction: ${partialFraction.toSage}")
            val transformed = extractedPartialAndTransformations
            transformed.map { transformedPF =>
              val pff = transformedPF.expression
              val renamed = rename(pff)
              if (hashSet.contains(renamed) && hashSet(renamed).head != theories(i).theory) {
                val m = map(theoryId).put(partialFraction, (renamed, transformedPF.transform, hashSet(renamed)) :: map(theoryId)(partialFraction))
                  logger.debug(m.toString())
                  m
              }
            }
          }
      }
    }

//    logger.debug("finished")
//    logger.debug(map.map(x => (x._1, x._2.map(y => (y._1.toSage, y._2.map(z => (z._1.toSage, z._2, z._3)))))).mkString("\n"))
//    logger.debug(s"\n\n\n")
//
//    logger.debug("Direct relations to be printed \n\n\n\n")
    val directRelations = map.filter(theoryMap => theoryMap._2.forall(_._2.nonEmpty) && theoryMap._2.nonEmpty)
//    logger.debug(directRelations.map(x => (x._1, x._2.map(y => (y._1.toSage, y._2.map(z => (z._1.toSage, z._2, z._3)))))).mkString("\n"))
//    logger.debug("\n\n\n\n")
//
//    logger.debug("Generating functions per sequence to be printed \n\n\n\n\n")
//    logger.debug("\n\n\n\n\n")
//

    val script = map.toList.sortBy(_._1.substring(1).toInt).map { case (k,v) =>
      s"""{ "name": "$k", "size": 10, "imports": [${v.flatMap(x => x._2.flatMap(y => y._3).distinct).map(x => "\"" + x + "\"").mkString(",")}] } """
    }.mkString(",")
//
    logger.debug("script being printed")
    logger.debug(script)

    logger.debug(s"Number of theories that can be expressed as direct relations: ${directRelations.size}")
    logger.debug(s"Number of unique sequence direct relations: ${directRelations.flatMap(theoryMap => theoryMap._2.flatMap(_._2.flatMap(_._3)).toList.distinct).toList.distinct.length}")
    logger.debug(s"Number of total direct relations: ${directRelations.map(theoryMap => theoryMap._2.map(_._2.flatMap(_._3).distinct.length).product).sum/2}")
    logger.debug(s"Number of shift/scale relations: ${hashSet.map(_._2.distinct.length - 1).sum}")
    logger.debug(s"Number of shift/scale relations: ${hashSet.map(x => (x._2.distinct.length*(x._2.distinct.length-1))/2).sum}")
//    logger.debug(hashSet.filter(x => x._2.length >1).map(x => (x._1.toSage, x._2)).mkString("\n"))
  }

  def produceCurrentGraph() = {
    val theories = DocumentDao.findAll().toArray
    println(s"Length is ${theories.length}")

    import parser.Expression._

    val graph = new mutable.HashMap[String, List[String]]()
    for(i <- theories.indices){
      val mainTheory = theories(i)
      println(i)
      val formulas = mainTheory.formulas.map(read[Sentence])
      val connections = formulas.flatMap(getSeqReference)
      connections.toList.distinct.map(graph.put(_, Nil))
      graph.put(mainTheory.theory, connections.toList.distinct)
//      connections.foreach { theory =>
//        val value = connections.filterNot(_ == theory).toList
//
//        if(graph.contains(theory)){
//          graph.put(theory, value ::: graph(theory))
//        }else{
//          graph.put(theory, value)
//        }
//      }
    }
    val script = graph.toList.sortBy(_._1.substring(1).toInt).map { case (k,v) =>
      s"""{ "name": "$k", "size": 10, "imports": [${v.map(x => "\"" + x + "\"").mkString(",")}] } """
    }.mkString(",")

    logger.debug("script being printed")
    logger.debug(script)
    logger.debug("Number of connections " + graph.count{ case (k,v) => v.length > 1})
  }



  def main(args: Array[String]): Unit = {
//    val expression = Div(List(Var("x"), Power(Sub(List(Num(1), Mul(List(Num(2), Var("x"))))), Num(2))))
//    val json = write(expression)
//    println(json)
//    println(read[Add](json))
//
//    println(SageWrapper.partialFraction(expression).get)
//    println(rename(SageWrapper.partialFraction(removeXMultiplications(removeConstants(expression))).get).toSage)
//
//    val expression1 = Iters("fff", None, None, Div(List(Var("x"), Power(Sub(List(Var("x"), Num(1))), Num(3)))))
//
//    println(SageWrapper.partialFraction(expression1))
//    println()
//
//    val legit = DocumentDao.findAll().toList.map { theory =>
//      val parsedTheory = theory.sageUnified.map { x =>
//        val parsed = read[PartialFractionToTransforms](x)
//        read[Expression](parsed.expression) -> parsed.transforms.map(_.map(read[PartialFractionAndTransform]))
//      }
//      parsedTheory.length
//    }.sum
//    println(s"Number of legit ordinary generating functions ${legit}")
    val b: Future[Boolean] = null

//    logger.debug("Number of generating functions: " + DocumentDao.findAll().toList.map(_.pureGeneratingFunctions.length).sum)
//    logger.debug(s"Generating functions ${DocumentDao.findAll().toList.map(x => x.pureGeneratingFunctions.map(read[Expression](_).toSage).mkString("\n")).mkString("\n\n\n")}")
//    populatePartialized()
    tryFunctionMatch()
//    logger.debug("Number of valid~ generating functions: "+ validGFS)

//    val expr = Div(Num(3)::Var("x")::Nil)
//    println(expr.toSage)
//    println(removeConstants(expr).toSage)
  }
}
