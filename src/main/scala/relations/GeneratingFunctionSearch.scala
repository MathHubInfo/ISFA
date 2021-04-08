package relations

import java.io.{BufferedWriter, FileWriter}
import java.util.concurrent.ConcurrentHashMap

import com.mongodb.casbah.commons.MongoDBObject
import org.slf4j.LoggerFactory
import parser._
import sage.SageWrapper

import scala.collection.JavaConverters._
import scala.collection.immutable.::
import scala.collection.mutable

import java.io.PrintWriter


object GeneratingFunctionSearch {

  def logger = LoggerFactory.getLogger(this.getClass)

  /**
   * dead code is commented out...
   */
  /*
    private val renameBase = "vari"
    val hashMap = new ConcurrentHashMap[Expression, (List[(String, String, String, String)])].asScala
    var validGFS = 0

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
      case Iters(name, from, to, on) =>
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
      val currentRenaming = collection.mutable.Map[String, String]()
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
        case Iters(name, from, to, on) => Iters(name,
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
        case Iters(name, from, to, on) =>
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
*/
  def extractPartialFractionSummands(partialFractions: Expression): List[Expression] = partialFractions match {
    case Add(expr) => expr.flatMap(extractPartialFractionSummands)
    case Sub(expr) =>
      val partials = expr.flatMap(extractPartialFractionSummands)
      partials.headOption.toList ::: partials.tail.map(Neg.apply)
    case x => x :: Nil
  }

  /*
      def makeTransformations(expression: Expression): List[(Expression, String)] = {
        val integrate = SageWrapper.integrate(expression)
        val differentiate = SageWrapper.derivative(expression)
        val same = expression

        (same, "unit") :: integrate.toList.map((_, "integrate")) ::: differentiate.toList.map((_, "differentiate"))
      }
  */
  def doTransformations(expression: Expression): List[(Expression, Transformation)] = {
    val integrate = SageWrapper.integrate(expression)
    val differentiate = SageWrapper.derivative(expression)
    val same = expression

    (same, Unit) :: integrate.toList.map((_, Integral)) ::: differentiate.toList.map((_, Differential))
  }

  /*
      def checkIfExists(expression: Expression, id: String) = {
        val partialFractionsOpt = SageWrapper.partialFraction(expression)
        partialFractionsOpt.foreach { partialFractions =>
          validGFS += 1
          val extractedGFs = extractPartialFractionSummands(partialFractions)
          val transformed = extractedGFs.flatMap(makeTransformations)
          transformed.foreach { gf =>
            val renamed = rename(removeXMultiplications(removeConstants(gf._1)))
            if (hashMap.contains(renamed)) {
              logger.debug(s"Partial fraction summand transformed already there ${renamed.toSage} with $id from \n ${expression.toSage} \n " +
                s"from partial fraction \n ${partialFractions.toSage} \n")
              hashMap.put(renamed, (id, gf._2, expression.toSage, partialFractions.toSage) :: hashMap(renamed))
            }
            else hashMap.put(renamed, (id, gf._2, expression.toSage, partialFractions.toSage) :: Nil)
          }
        }
      }

      def equal(left: Expression, right: Expression) = {
        //    println(s"checking equality of $left and $right")
        logger.debug(s"gfs - checking equality of $left and $right")

        rename(left) == rename(right)
      }
  */
  def removeNegation(expression: Expression): Expression = expression match {
    case Neg(a) => removeNegation(a)
    case Mul(list) => Mul(list.map(removeNegation))
    case Div(list) => Div(list.map(removeNegation))
    case Sub(list) => Sub(list.map(removeNegation))
    case Add(list) => Add(list.map(removeNegation))
    case x => x
  }

  def removeConstants(expression: Expression): Expression = removeNegation(expression) match {
    case Mul(Num(a) :: b :: Nil) => b
    case Mul(Num(a) :: b) => removeConstants(Mul(b))
    case Mul(Div(Num(a) :: Num(b) :: Nil) :: c :: nil) => removeConstants(c)
    case Div(Num(a) :: Num(b) :: c :: Nil) => Div(Num(1) :: c :: Nil)
    case Div(Num(a) :: c :: Nil) => Div(Num(1) :: c :: Nil)
    case Div(Mul(Num(a) :: b :: Nil) :: c :: Nil) => Div(b :: c :: Nil)
    //    case Div(list) if list.length > 3 => throw new Exception(s"Cannot deal with ${list}")
    case x => x
  }

  /*
      def removeConstantsWithFeedback(expression: Expression): (Expression, Double) = removeNegation(expression) match {
        case Mul(Num(a) :: b :: Nil) => (b, a)
        case Mul(Num(a) :: b) =>
          val result = removeConstantsWithFeedback(Mul(b))
          (result._1, result._2 * a)
        case Mul(Div(Num(a) :: Num(b) :: Nil) :: c :: nil) =>
          val result = removeConstantsWithFeedback(c)
          (result._1, result._2 * (a / b))
        case Div(Num(a) :: Num(b) :: c :: Nil) => (Div(Num(1) :: c :: Nil), a / b)
        case Div(Num(a) :: c :: Nil) => (Div(Num(1) :: c :: Nil), a)
        case Div(Mul(Num(a) :: b :: Nil) :: c :: Nil) => (Div(b :: c :: Nil), a)
        //    case Div(list) if list.length > 3 => throw new Exception(s"Cannot deal with ${list}")
        case x => (x, 1)
      }
      */
  def removeXMultiplications(expression: Expression): Expression = removeNegation(expression) match {
    case Div(Power(Var("x"), exp) :: b :: Nil) => Div(Num(1) :: b :: Nil)
    case Div(Var("x") :: exp :: Nil) => Div(Num(1) :: exp :: Nil)
    case x => x
  }

  /*
      def removeXMultiplicationsWithFeedback(expression: Expression): (Expression, Double) = removeNegation(expression) match {
        case Div(Power(Var("x"), Num(exp)) :: b :: Nil) => Div(Num(1) :: b :: Nil) -> exp
        case Div(Var("x") :: exp :: Nil) => Div(Num(1) :: exp :: Nil) -> 1
        case x => x -> 0
      }

      def normalizeFractions(expression: Expression): Expression = expression match {
        case Div(Num(constant) :: Power(Sub(Num(1) :: Mul(bx) :: Nil), Num(k)) :: Nil) =>
          Div(Num(1) :: Sub(Num(1) :: removeConstants(Mul(bx)) :: Nil) :: Nil)
        case Div(Num(constant) :: Sub(Num(1) :: Mul(bx) :: Nil) :: Nil) =>
          Div(Num(1) :: Sub(Num(1) :: removeConstants(Mul(bx)) :: Nil) :: Nil)
        case x => x
      }

    */

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

    def getShiftAndMultiplication(multConst: Expression): (Double, Expression) = {
      multConst match {
        case Mul(Var("x") :: Nil) => (-1, Num(1))
        case Mul(Num(c) :: b) =>
          val temp = getShiftAndMultiplication(Mul(b))
          (temp._1, Mul(temp._2 :: Num(c) :: Nil))
        case Mul(Power(Var("x"), Num(sConstant)) :: Nil) => (-sConstant, Num(1))
        case Mul(Div(a) :: b) =>
          val temp = getShiftAndMultiplication(Div(a))
          val temp1 = getShiftAndMultiplication(Mul(b))
          (temp._1 + temp1._1, Mul(temp._2 :: temp1._2 :: Nil))

        case Div(Num(c) :: b) =>
          val temp = getShiftAndMultiplication(Mul(b))
          (-temp._1, Div(Num(c) :: temp._2 :: Nil))
        case Div(Var("x") :: Nil) => (1, Num(1))
        case Div(Power(Var("x"), Num(sConstant)) :: Nil) => (sConstant, Num(1))
        case Neg(expr) =>
          val result = getShiftAndMultiplication(expr)
          result._1 -> Neg(result._2)
        case _ => (0, Num(1))
      }
    }

    val (shiftFromTheMultiplication, multiplicationConstantFromTheMultiplication) =
      getShiftAndMultiplication(simplifiedConst)

    val finalShifts = (Mul(multiplicationConstant :: {
      if (multiplicationConstantFromTheMultiplication != Num(1))
        multiplicationConstantFromTheMultiplication :: Nil
      else
        Nil
    }), Add(Num(shiftConstant + shiftFromTheMultiplication) :: Nil))

    val simplifiedFinalShifts = (SageWrapper.simplifyFull(finalShifts._1).orElse(SageWrapper.simplify(finalShifts._1)), SageWrapper.simplify(finalShifts._2))

    simplifiedFinalShifts match {
      case (Some(multiplication), Some(shift)) =>
        Some(Mul(multiplication :: FuncR(SeqReference(theory), ArgList(Add(Var("n") :: {
          if (shift == Num(0)) Nil else shift :: Nil
        }) :: Nil)) :: Nil))
      case (None, Some(shift)) =>
        Some(Mul(finalShifts._1 :: FuncR(SeqReference(theory), ArgList(Add(Var("n") :: {
          if (shift == Num(0)) Nil else shift :: Nil
        }) :: Nil)) :: Nil))

      case _ =>
        //println(s"Couldn't simplify ${finalShifts._1.toSage} and ${finalShifts._2.toSage}")
        logger.debug(s"gfs - Couldn't simplify ${finalShifts._1.toSage} and ${finalShifts._2.toSage}")
        None
    }
  }

  /**
   * This method tries to find relations such that each partial fraction can be expressed through an existing OEIS sequence.
   * Explained in the B.Sc. Thesis https://github.com/MathHubInfo/ISFA/tree/master/docs
   */
  def secondMethod() = {


    case class MappedTheory(
                             theory: TheoryRep,
                             initialGeneratingFunction: Expression,
                             unifiedGeneratingFunction: Expression,
                             unificationConstant: Option[Expression]
                           )

    val hashMap = new ConcurrentHashMap[Expression, List[MappedTheory]]().asScala
    val theories = TheoryRepDao.findAll().toArray
    //println(s"Length is ${theories.length}")
    logger.debug(s"gfs - Length is ${theories.length}")

    val indices = theories.indices

    //for (i <- indices) {
    for (i <- 0 to 1) {
      val theoryRep = theories(i)
      //println(i)
      logger.debug(s"gfs - $i")


      theoryRep.generatingFunctions.foreach { generatingFunction =>
        for (
          simplifiedGeneratingFunction <- SageWrapper.simplifyFull(generatingFunction);
          unifiedGeneratingFunction = removeXMultiplications(removeConstants(simplifiedGeneratingFunction));
          simplifiedUnifiedGeneratingFunction <- SageWrapper.simplifyFull(unifiedGeneratingFunction)
        ) {
          val mappedTheory = MappedTheory(
            theory = theoryRep,
            initialGeneratingFunction = generatingFunction,
            unifiedGeneratingFunction = simplifiedUnifiedGeneratingFunction,
            unificationConstant = SageWrapper.divide(generatingFunction, unifiedGeneratingFunction)
          )
          // NOTE TODO Commenting these lines makes the second method only take ONE sequence instance
          // out of the many that unify under method 1. This to avoid the transitional relations
          // that come from it
          //          if (hashMap.contains(simplifiedUnifiedGkeneratingFunction)) {
          //            hashMap.put(simplifiedUnifiedGeneratingFunction, mappedTheory :: hashMap(simplifiedUnifiedGeneratingFunction))
          //          } else
          hashMap.put(simplifiedUnifiedGeneratingFunction, List(mappedTheory))
        }
      }

      val hashMapSize = hashMap.size
      logger.debug(s"hashMapSize : $hashMapSize")
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

    var gen_count: Long = 0
    var rep_count: Long = 0
    //    val map = new mutable.HashSet[FullExpressingTheory]()
    for (i <- indices) {
      logger.debug(s"Processing theory $i")
      val generatingFunctions = theories(i).generatingFunctions
      generatingFunctions.foreach { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction).map(extractPartialFractionSummands)

        partialFractionsOpt.foreach { partialFractions =>
          val partialExpressingTheories = partialFractions.map { partialFraction =>
            doTransformations(partialFraction).flatMap { case (transformedPartialFraction, transformation) =>
              SageWrapper.simplifyFull(transformedPartialFraction).flatMap { simplifiedTransformedPartialFraction =>
                val unifiedTransformedPartialFraction = removeXMultiplications(removeConstants(transformedPartialFraction))
                SageWrapper.simplifyFull(unifiedTransformedPartialFraction).flatMap { simplifiedUnifiedTransformedPartialFraction =>

                  if (hashMap.get(simplifiedUnifiedTransformedPartialFraction).exists(_.nonEmpty) && hashMap(simplifiedUnifiedTransformedPartialFraction).head.theory.theory != theories(i).theory) {
                    val relations = hashMap(simplifiedUnifiedTransformedPartialFraction).flatMap { mappedTheory =>
                      mappedTheory.unificationConstant.flatMap { leftUnificationFactor =>
                        SageWrapper.divide(transformedPartialFraction, simplifiedUnifiedTransformedPartialFraction).flatMap { rightUnificationFactor =>
                          val constN = Div(rightUnificationFactor :: leftUnificationFactor :: Nil)
                          SageWrapper.simplifyFull(constN).orElse(SageWrapper.simplify(constN)).map { simplifiedConst =>

                            val relation = Func(transformation.inverse.toString, ArgList(Mul(simplifiedConst :: SeqReference(mappedTheory.theory.theory) :: Nil) :: Nil))

                            Relation(
                              partialFractionSubstitution = relation,
                              partialFractionSubstitutionRepresentingFunctionLevel = getRepresentingFunction(transformation.inverse, simplifiedConst, mappedTheory.theory.theory).get,
                              unificationConstant = simplifiedConst,
                              transformation = transformation.inverse
                            )
                          }
                        }
                      }
                    }

                    if (relations != Nil) {
                      Some(PartialExpressingTheory(
                        theories(i).theory,
                        partialFraction,
                        transformedPartialFraction,
                        transformation,
                        partialFractions.filterNot(_ == partialFraction),
                        hashMap(simplifiedUnifiedTransformedPartialFraction),
                        relations
                      ))
                    } else None
                  } else None
                }
              }

            }
          }

          if (partialExpressingTheories.forall(_.nonEmpty)) {
            val fullExpressionTheory = FullExpressingTheory(theories(i).theory, partialExpressingTheories, partialFractions)
            val total = fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitution)).length).product
            if (total > 300000) { // for optimization purposes
              //              combine(fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitution))), { partialFractionsAsRelations =>
              //                val relation = Equation("=", SeqReference(fullExpressionTheory.theoryId), Add(partialFractionsAsRelations))
              //                val rel = RelationRep(2, RelationRep.generatingFunction, relation)
              //                gen_count += 1
              //                RelationDao.insert(rel)
              //              })
              combine(fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitutionRepresentingFunctionLevel))), { partialFractionsAsRelations =>
                val relation = Equation("=", SeqReference(fullExpressionTheory.theoryId), Add(partialFractionsAsRelations))
                val rel = RelationRep(2, RelationRep.representingFunction, relation)
                rep_count += 1
                RelationDao.insert(rel)
              })
            } else {
              //              val gen = combineMem(fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitution)))).map { partialFractionsAsRelations =>
              //                val relation = Equation("=", SeqReference(fullExpressionTheory.theoryId), Add(partialFractionsAsRelations.toList))
              //                RelationRep(2, RelationRep.generatingFunction, relation)
              //              }
              //              gen_count += gen.length
              //              RelationDao.insert(gen)

              val rep = combineMem(fullExpressionTheory.expressingPartials.map(_.flatMap(_.relations.map(_.partialFractionSubstitutionRepresentingFunctionLevel)))).map { partialFractionsAsRelations =>
                val relation = Equation("=", SeqReference(fullExpressionTheory.theoryId), Add(partialFractionsAsRelations.toList))
                RelationRep(2, RelationRep.representingFunction, relation)
              }

              rep_count += rep.length
              RelationDao.insert(rep)
            }

          }
        }
      }
    }

    def combine(xs: List[List[Expression]], onGenerate: List[Expression] => Unit, acc: List[Expression] = Nil): Unit = {
      xs match {
        case a :: b =>
          a.foreach { elem =>
            combine(b, onGenerate, elem :: acc)
          }
        case Nil =>
          onGenerate(acc)
      }
    }

    def combineMem[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] =
      xs.foldLeft(Seq(Seq.empty[A])) {
        (x, y) => for (a <- x; b <- y) yield a :+ b
      }

    logger.debug(s"Count: ${gen_count}   ${rep_count}")
  }

  //  def countRelations() = {
  //    val relations = RelationDao.find(MongoDBObject("method" -> 2))
  //
  //    val counting = relations.toList.groupBy { _.expression match { case Equation (_, SeqReference(id), _) => id }}
  //      .map(x => x._1 -> x._2.length)
  //    logger.debug(s"Relations")
  //    val groupedRelations = relations.toList.groupBy { _.expression match {case Equation(_, SeqReference(id), _) => id }}
  //      .values.toList.flatten.filter(_.level == RelationRep.representingFunction)
  //    logger.debug(s"Length ${groupedRelations.size}")
  //    groupedRelations.foreach(x => logger.debug(s"\n${x.expression.toSage} \n"))
  //    counting.foreach(x => logger.debug(x.toString()))
  //  }

  /**
   * This method tries to find one partial fraction intersection.
   * Explained in the B.Sc. Thesis https://github.com/MathHubInfo/ISFA/tree/master/docs
   */
  def thirdMethod() = {
    case class MappedTheory(
                             theory: TheoryRep,
                             initialGeneratingFunction: Expression,
                             initialPartialFraction: Expression,
                             unifiedPartialFraction: Expression,
                             restOfPartialFractions: List[Expression]
                           )

    val hashMap = new mutable.HashMap[Expression, List[MappedTheory]]()
    val theories = TheoryRepDao.findAll().toArray
    //println(s"Length is ${theories.length}")
    logger.debug(s"gfs - Length is ${theories.length}")


    val indices = theories.indices
    for (i <- indices) {
      val theoryRep = theories(i)
      //println(i)
      logger.debug(s"gfs - $i")
      theoryRep.generatingFunctions.foreach { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction).map(extractPartialFractionSummands)

        partialFractionsOpt.foreach(partialFractions =>
          partialFractions.foreach { partialFraction =>
            SageWrapper.simplifyFull(partialFraction).foreach { simplifiedPartialFraction =>
              val unifiedPartialFraction = removeXMultiplications(removeConstants(simplifiedPartialFraction))
              SageWrapper.simplifyFull(unifiedPartialFraction).foreach { simplifiedUnifiedPartialFraction =>
                val mappedTheory = MappedTheory(
                  theory = theoryRep,
                  initialGeneratingFunction = generatingFunction,
                  initialPartialFraction = partialFraction,
                  unifiedPartialFraction = simplifiedUnifiedPartialFraction,
                  restOfPartialFractions = partialFractions.filterNot(_ == partialFraction)
                )
                // TODO: SEE ABOVE, SAME STORY
                //                if (hashMap.contains(unifiedPartialFraction)) {
                //                  hashMap.put(unifiedPartialFraction, mappedTheory :: hashMap(unifiedPartialFraction))
                //                } else
                hashMap.put(unifiedPartialFraction, List(mappedTheory))
              }
            }
          }
        )

      }
      val hashMapSize = hashMap.size
      logger.debug(s"hashMapSize : $hashMapSize")
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
      //println(s"Processing theory $i")
      logger.debug(s"gfs - Processing theory $i")
      val generatingFunctions = theories(i).generatingFunctions
      generatingFunctions.foreach { generatingFunction =>
        val partialFractionsOpt = SageWrapper.partialFraction(generatingFunction).map(extractPartialFractionSummands)

        partialFractionsOpt.foreach { partialFractions =>
          partialFractions.foreach { partialFraction =>
            doTransformations(partialFraction).foreach { case (transformedPartialFraction, transformation) =>
              SageWrapper.simplifyFull(transformedPartialFraction).foreach { simplifiedTransformedPartialFraction =>
                val unifiedTransformedPartialFraction = removeXMultiplications(removeConstants(simplifiedTransformedPartialFraction))
                SageWrapper.simplifyFull(unifiedTransformedPartialFraction).foreach { simplifiedUnifiedTransformedPartialFraction =>
                  SageWrapper.divide(transformedPartialFraction, simplifiedUnifiedTransformedPartialFraction)
                    .foreach { transformedDivisionFactor =>

                      if (hashMap.contains(simplifiedUnifiedTransformedPartialFraction) && hashMap(simplifiedUnifiedTransformedPartialFraction).filter(_
                        .theory.theory != theories(i).theory) != Nil) {
                        val expressionTheory =
                          ExpressingTheory(
                            theoryId = theories(i).theory,
                            partialFraction = partialFraction,
                            transformedPartialFunction = simplifiedUnifiedTransformedPartialFraction,
                            transformation = transformation,
                            restOfPartialFractions = partialFractions.filterNot(_ == partialFraction),
                            mappedTheories = hashMap(simplifiedUnifiedTransformedPartialFraction)
                          )
                        expressionTheory.mappedTheories.foreach { mapTheory =>
                          val IUPFROpt = SageWrapper.divide(mapTheory.initialPartialFraction, mapTheory.unifiedPartialFraction)
                          //println(IUPFROpt)
                          logger.debug(s"gfs - $IUPFROpt")
                          IUPFROpt.foreach { IUPFR =>
                            val negRestofPartials = expressionTheory.restOfPartialFractions match {
                              case Nil => Nil
                              case r => List(Neg(Add(r)))
                            }
                            val substitution = Add(SeqReference(expressionTheory.theoryId) :: negRestofPartials)

                            SageWrapper.simplifyFull(Div(IUPFR :: transformedDivisionFactor :: Nil)).foreach {
                              simplifiedConstant =>

                                val relation = Equation(
                                  "=",
                                  SeqReference(mapTheory.theory.theory),
                                  Add(
                                    Mul(simplifiedConstant :: Func(expressionTheory.transformation
                                      .toString, ArgList(List
                                    (substitution))) :: Nil) :: mapTheory.restOfPartialFractions
                                  )
                                )

                                RelationDao.insert(RelationRep(3, RelationRep.generatingFunction, relation))
                                relation
                            }
                          }
                        }
                      }
                    }
                }
              }
            }
          }
        }
      }
    }

    //    logger.debug("\n\n\n\n\n\nPrinting relations at the generating functions: \n\n\n\n\n")
    //    logger.debug(relations.filter(_.isDefined).map(_.get.toSage).mkString("\n \n"))
  }

  //  def produceCurrentGraph() = {
  //    val theories = DocumentDao.findAll().toArray
  //    println(s"Length is ${theories.length}")
  //
  //    import parser.Expression._
  //
  //    val graph = new mutable.HashMap[String, List[String]]()
  //    for(i <- theories.indices){
  //      val mainTheory = theories(i)
  //      println(i)
  //      val formulas = mainTheory.formulas.map(read[Sentence])
  //      val connections = formulas.flatMap(getSeqReference)
  //      connections.toList.distinct.map(graph.put(_, Nil))
  //      graph.put(mainTheory.theory, connections.toList.distinct)
  //      connections.foreach { theory =>
  //        val value = connections.filterNot(_ == theory).toList
  //
  //        if(graph.contains(theory)){
  //          graph.put(theory, value ::: graph(theory))
  //        }else{
  //          graph.put(theory, value)
  //        }
  //      }
  //    }
  //    val script = graph.toList.sortBy(_._1.substring(1).toInt).map { case (k,v) =>
  //      s"""{ "name": "$k", "size": 10, "imports": [${v.map(x => "\"" + x + "\"").mkString(",")}] } """
  //    }.mkString(",")
  //
  //    logger.debug("script being printed")
  //    logger.debug(script)
  //    logger.debug("Number of connections " + graph.count { case (k,v) => v.length > 1})
  //  }

  def saveRelationsToFile(method: Int) = {
    val ioFileName = s"relations-$method"
    val file = new FileWriter(ioFileName)
    file.write("")
    val bufferedFile = new BufferedWriter(file, 1024 * 1024 * 500)
    val relationTransformation =
    //ToDo: What is this? And why is it like this?
      if (method == 2) {
        relation: Expression =>
          relation match {
            case Equation("=", SeqReference(seq), rest) =>
              Equation("=", FuncR(SeqReference(seq), ArgList(List(Var("n")))), rest)
          }
      } else {
        relation: Expression => relation
      }

    val relationQuery = RelationDao.find(MongoDBObject("method" -> method)).batchSize(400000)
    val relations = new Iterator[RelationRepString] {
      override def hasNext: Boolean = relationQuery.hasNext

      override def next(): RelationRepString = relationQuery.next()
    }

    relations.foreach { relation =>
      bufferedFile.append(relationTransformation(FormulaParserInst.parse(relation.expression).get).toSage + "\n")
    }
    bufferedFile.flush()
    bufferedFile.close()
  }


  def export_to_MathML() = {

    val theories = TheoryRepDao.findAll().toArray
    logger.debug(s"gfs - Length is ${theories.length}")

    val indices = theories.indices

    for (i <- indices) {
      val theoryRep = theories(i)
      logger.debug(s"gfs - $i")
      logger.debug("Working Directory = " + System.getProperty("user.dir"))

      var j = 0
      val num_gfs = theoryRep.generatingFunctions.size

      theoryRep.generatingFunctions.foreach {generatingFunction =>
        //logger.debug(s"${theoryRep.name} gfs ${generatingFunction.toSage}")
        new PrintWriter("./results/gfs/2020/" + theoryRep.theory + "_"+j.toString + ".xml") {
          write("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n");
          logger.debug(s"${theoryRep.name} gfs ${generatingFunction.toCML}")
          write(generatingFunction.toCML.toString());
          write("\n</math>")
          j = j+1
          close
        }


      }
      j = 0;
    }
  }


  def main(args: Array[String]): Unit = {
    /* //This is just a test, right?
    val expression = Div(List(Var("x"), Power(Sub(List(Num(95), Mul(List(Num(15), Var("x"))))), Num(5))))
    println(SageWrapper.partialFraction(expression).get)
    println(rename(SageWrapper.partialFraction(removeXMultiplications(removeConstants(expression))).get).toSage)
*/
    //secondMethod()
    //thirdMethod()

    //debug function to reconstruct the content MathML export
    export_to_MathML()

    //saveRelationsToFile(2)
    //    println("Finished")

    // logger.debug(getRepresentingFunction(Integral, FormulaParserInst.parse("-((1/10/x))").get, "").get.toSage)
  }
}
