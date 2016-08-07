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
    case Sub(expr) =>
      val partials = expr.flatMap(extractPartialFractionSummands)
      partials.headOption.toList ::: partials.tail.map(Neg.apply)
    case x => x :: Nil
  }

  def makeTransformations(expression: Expression): List[(Expression,String)] = {
    val integrate = SageWrapper.integrate(expression)
    val differentiate = SageWrapper.derivative(expression)
    val same = expression

    (same, "unit") :: integrate.toList.map((_, "integrate")) ::: differentiate.toList.map((_, "differentiate"))
  }

  def doTransformations(expression: Expression): List[(Expression,Transformation)] = {
    val integrate = SageWrapper.integrate(expression)
    val differentiate = SageWrapper.derivative(expression)
    val same = expression

    (same, Unit) :: integrate.toList.map((_, Integral)) ::: differentiate.toList.map((_, Differential))
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


  sealed trait Transformation {
    def inverse: Transformation
    def toString: String
  }
  object Integral extends Transformation {
    def inverse = Differential
    override def toString = "integrate"
  }
  object Differential extends Transformation {
    def inverse = Integral
    override def toString = "differential"
  }
  object Unit extends Transformation {
    def inverse = Unit
    override def toString = ""
  }

  def getRepresentingFunction(inverse: Transformation, simplifiedConst: Expression, theory: String): Option[Expression] = {

    val (multiplicationConstant, shiftConstant) = inverse match {
      case Integral => Div(Num(1) :: Var("n") :: Nil) -> -1
      case Differential => Add(Var("n") :: Num(1) :: Nil) -> 1
      case Unit => Num(1) -> 0
    }

    val (shiftFromTheMultiplication, multiplicationConstantFromTheMultiplication) = simplifiedConst match {
      case Mul(Num(c) :: Var("x") :: Nil) => (Num(-1), c)
      case Mul(Num(c) :: Power(Var("x"), sConstant) :: Nil) => (Neg(sConstant), c)
      case Div(Num(c) :: Var("x") :: Nil) => (Num(1), c)
      case Div(Num(c) :: Power(Var("x"), sConstant) :: Nil) => (sConstant, c)
      case Mul(Var("x") :: Nil) => (Num(-1), 1.0)
      case Mul(Power(Var("x"), sConstant) :: Nil) => (Neg(sConstant), 1.0)
      case _ => (Num(0), 1.0)
    }

    val finalShifts = (Mul(multiplicationConstant :: {
      if(multiplicationConstantFromTheMultiplication != 1)
        Num(multiplicationConstantFromTheMultiplication)::Nil
      else
        Nil
    }), Add(Num(shiftConstant) :: shiftFromTheMultiplication :: Nil))

    val simplifiedFinalShifts = (Some(finalShifts._1), SageWrapper.simplify(finalShifts._2))

    simplifiedFinalShifts match {
      case (Some(multiplication), Some(shift)) =>
        Some(Mul(multiplication :: FuncR(SeqReference(theory), ArgList(Add(Var("n") :: { if(shift == Num(0)) Nil else shift  :: Nil }) :: Nil)) :: Nil))
      case _ =>
        println(s"Couldn't simplify ${finalShifts._1.toSage} and ${finalShifts._2.toSage}")
        None
    }
  }

  def secondMethod() = {
    case class MappedTheory(
      theory: TheoryRep,
      initialGeneratingFunction: Expression,
      unifiedGeneratingFunction: Expression,
      unificationConstant: Option[Expression]
    )

    val hashMap = new mutable.HashMap[Expression, List[MappedTheory]]()
    val theories = TheoryRepDao.findAll().toArray.take(1000)
    println(s"Length is ${theories.length}")

    import parser.Expression._

    val indices = theories.indices

    for (i <- indices) {
      val theoryRep = theories(i)
      println(i)
      theoryRep.generatingFunctions.foreach { generatingFunction =>
        for (
        simplifiedGeneratingFunction <- SageWrapper.simplifyFull(generatingFunction);
        unifiedGeneratingFunction = removeXMultiplications(removeConstants(simplifiedGeneratingFunction));
        simplifiedUnifiedGeneratingFunction <- SageWrapper.simplifyFull(unifiedGeneratingFunction)
        )
        {
          val mappedTheory = MappedTheory(
            theory = theoryRep,
            initialGeneratingFunction = generatingFunction,
            unifiedGeneratingFunction = simplifiedUnifiedGeneratingFunction,
            unificationConstant = SageWrapper.divide(generatingFunction, unifiedGeneratingFunction)
          )
          if (hashMap.contains(simplifiedUnifiedGeneratingFunction)) {
            hashMap.put(simplifiedUnifiedGeneratingFunction, mappedTheory :: hashMap(simplifiedUnifiedGeneratingFunction))
          } else hashMap.put(simplifiedUnifiedGeneratingFunction, List(mappedTheory))
        }
      }
    }

    case class PartialExpressingTheory(
      theoryId: String,
      partialFraction: Expression,
      transformedPartialFunction: Expression,
      transformation: Transformation,
      restOfPartialFractions: List[Expression],
      mappedTheories: List[MappedTheory],
      relations: List[Relation]
    )

    case class Relation(
      partialFractionSubstitution: Expression, // integral( [[ unificationConst ]] * A12321)
      partialFractionSubstitutionRepresentingFunctionLevel: Expression,
      unificationConstant: Expression,
      transformation: Transformation
    )

    case class FullExpressingTheory(
      theoryId: String,
      expressingPartials: List[List[PartialExpressingTheory]],
      partialFractions: List[Expression]
    )

    val map = new mutable.HashSet[FullExpressingTheory]()
    for (i <- indices) {
      println(s"Processing theory $i")
      val generatingFunctions = theories(i).generatingFunctions
      generatingFunctions.foreach { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction).map(extractPartialFractionSummands)

        partialFractionsOpt.foreach { partialFractions =>
         val partialExpressingTheories = partialFractions.map { partialFraction =>
           doTransformations(partialFraction).flatMap { case (transformedPartialFraction, transformation) =>
             SageWrapper.simplifyFull(transformedPartialFraction).flatMap { simplifiedTransformedPartialFraction =>
               val unifiedTransformedPartialFraction = removeXMultiplications(removeConstants(transformedPartialFraction))

               if (hashMap.contains(simplifiedTransformedPartialFraction) && hashMap(simplifiedTransformedPartialFraction).head.theory.theory != theories(i).theory) {
                 val relations = hashMap(simplifiedTransformedPartialFraction).map { mappedTheory =>
                   mappedTheory.unificationConstant.flatMap { leftUnificationFactor =>
                     SageWrapper.divide(transformedPartialFraction, unifiedTransformedPartialFraction).map { rightUnificationFactor =>
                       val constN = Div(rightUnificationFactor :: leftUnificationFactor :: Nil)
                       val simplifiedConst = SageWrapper.simplifyFull(constN).orElse(SageWrapper.simplify(constN)).get

                       val relation = Func(transformation.inverse.toString, ArgList(Mul(simplifiedConst :: SeqReference(mappedTheory.theory.theory) :: Nil) :: Nil))

                       Relation(
                         partialFractionSubstitution = relation,
                         partialFractionSubstitutionRepresentingFunctionLevel = getRepresentingFunction(transformation.inverse, simplifiedConst, mappedTheory.theory.theory).get,
                         unificationConstant = simplifiedConst,
                         transformation = transformation.inverse
                       )
                     }
                   }
                 }.collect { case Some(x) => x }

                 Some(PartialExpressingTheory(
                   theories(i).theory,
                   partialFraction,
                   transformedPartialFraction,
                   transformation,
                   partialFractions.filterNot(_ == partialFraction),
                   hashMap(simplifiedTransformedPartialFraction),
                   relations
                 ))
               } else None
             }
           }
         }

          if(partialExpressingTheories.forall(_.nonEmpty)) {
            map += FullExpressingTheory(theories(i).theory, partialExpressingTheories, partialFractions)
          }
        }
      }
    }

    def combine[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
      xs.foldLeft(Seq(Seq.empty[A])){
        (x, y) => for (a <- x.view; b <- y) yield a :+ b
      }

    val allRelations = map.toList.flatMap { fullExpressionTheory =>
      combine(fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitution)))).map { partialFractionsAsRelations =>
        Equation("=", SeqReference(fullExpressionTheory.theoryId), Add(partialFractionsAsRelations.toList))
      }
    }

    val allRelationRepresentingFunction = map.toList.flatMap { fullExpressionTheory =>
      combine(fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitutionRepresentingFunctionLevel)))).map { partialFractionsAsRelations =>
        Equation("=", SeqReference(fullExpressionTheory.theoryId), Add(partialFractionsAsRelations.toList))
      }
    }

    println(allRelations.map(_.toSage).mkString("\n \n"))
    println("\n\n\n\n\n\nPrinting relations at the representing functions: \n\n\n\n\n")
    println(allRelationRepresentingFunction.map(_.toSage).mkString("\n \n"))
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
    case class MappedTheory(
      theory: TheoryRep,
      initialGeneratingFunction: Expression,
      initialPartialFraction: Expression,
      unifiedPartialFraction: Expression,
      restOfPartialFractions: List[Expression]
    )

    val hashMap = new mutable.HashMap[Expression, List[MappedTheory]]()
    val theories = TheoryRepDao.findAll().toArray.take(100)
    println(s"Length is ${theories.length}")

    import parser.Expression._

    val indices = theories.indices

    for (i <- indices) {
      val theoryRep = theories(i)
      println(i)
      theoryRep.generatingFunctions.foreach { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction).map(extractPartialFractionSummands)

        partialFractionsOpt.foreach(partialFractions =>
          partialFractions.foreach { partialFraction =>
            val unifiedPartialFraction = removeXMultiplications(removeConstants(partialFraction))
            val mappedTheory = MappedTheory(
              theory = theoryRep,
              initialGeneratingFunction = generatingFunction,
              initialPartialFraction = partialFraction,
              unifiedPartialFraction = unifiedPartialFraction,
              restOfPartialFractions = partialFractions.filterNot(_ == partialFraction)
            )
            if (hashMap.contains(unifiedPartialFraction)) {
              hashMap.put(unifiedPartialFraction, mappedTheory :: hashMap(unifiedPartialFraction))
            } else hashMap.put(unifiedPartialFraction, List(mappedTheory))
          }
        )

      }
    }

    case class ExpressingTheory(
      theoryId: String,
      partialFraction: Expression,
      transformedPartialFunction: Expression,
      transformation: Transformation,
      restOfPartialFractions: List[Expression],
      mappedTheories: List[MappedTheory]
    )

    val map = new mutable.HashMap[String, List[ExpressingTheory]]()
    for (i <- indices) {
      println(s"Processing theory $i")
      val generatingFunctions = theories(i).generatingFunctions
      generatingFunctions.foreach { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction).map(extractPartialFractionSummands)

        partialFractionsOpt.foreach { partialFractions =>
          partialFractions.foreach { partialFraction =>
            doTransformations(partialFraction).foreach { case (transformedPartialFraction, transformation) =>
              if (hashMap.contains(transformedPartialFraction) && hashMap(transformedPartialFraction).head.theory.theory != theories(i).theory) {
                map.put(
                  theories(i).theory,
                  ExpressingTheory(
                    theories(i).theory,
                    partialFraction,
                    transformedPartialFraction,
                    transformation,
                    partialFractions.filterNot(_ == partialFraction),
                    hashMap(transformedPartialFraction)
                  ) :: map.getOrElse(theories(i).theory, Nil)
                )
              }
            }
          }
        }
      }
    }

    val relations = map.values.flatten.flatMap { expressionTheory =>
      expressionTheory.mappedTheories.map { mapTheory =>
        val IUPFROpt = SageWrapper.divide(mapTheory.initialPartialFraction, mapTheory.unifiedPartialFraction)
        println(IUPFROpt)
        IUPFROpt.map { IUPFR =>
          val substitution = Add(List(SeqReference(expressionTheory.theoryId), Neg(Add(expressionTheory.restOfPartialFractions))))
          Equation(
            "=",
            SeqReference(mapTheory.theory.theory),
            Add(
              Mul(IUPFR :: Func(expressionTheory.transformation.toString, ArgList(List(substitution))) :: Nil) :: mapTheory.restOfPartialFractions
            )
          )
        }
      }
    }

    logger.trace(relations.filter(_.isDefined).map(_.get.toSage).mkString("\n \n"))
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
    logger.debug("Number of connections " + graph.count { case (k,v) => v.length > 1})
  }



  def main(args: Array[String]): Unit = {
    val expression = Div(List(Var("x"), Power(Sub(List(Num(1), Mul(List(Num(2), Var("x"))))), Num(2))))

    println(SageWrapper.partialFraction(expression).get)
    println(rename(SageWrapper.partialFraction(removeXMultiplications(removeConstants(expression))).get).toSage)
    secondMethod()

    TheoryRepDao.findOneByTheory(217).map(x => println(x.generatingFunctions.map(_.toSage)))
  }
}