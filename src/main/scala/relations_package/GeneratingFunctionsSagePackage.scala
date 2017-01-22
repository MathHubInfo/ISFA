package relations_package

import scala.collection.mutable.ListBuffer
import scala.reflect.io.File

import org.slf4j.LoggerFactory

import parser._
import sage.SageWrapper

object GeneratingFunctionsSagePackage {
  lazy val logger = LoggerFactory.getLogger(this.getClass)
  lazy val sequenceLogger = LoggerFactory.getLogger(SageWrapper.getClass)

  def s(generatingFunctions: Seq[Expression], theory: String) = {
    s"""
       |    for generatingFunction in ultimate_question.${theory}_gen():
       |        sequence = map(lambda xy: xy[0], generatingFunction.series(x, 11).coefficients())
       |        sequence.pop(0)
       |        sequence.pop(0)
       |        sequence.pop(0)
       |        sequence.pop(0)
       |        originalSequence = oeis('$theory').first_terms()
       |        if not ultimate_question.contains_sublist(list(originalSequence), sequence):
       |            print(generatingFunction, '$theory')
     """.stripMargin
  }

  def generateVerificationScripts() = {
    val outputFile = File("verifications").printWriter()
    TheoryRepDao.findAll().foreach { theory =>
      val generatingFunctions = theory.generatingFunctions.map(rename).filter { generatingFunction =>
        val simplified = SageWrapper.simplifyFull(generatingFunction)
        simplified.exists {
          case Num(constant) => false
          case Mul(Num(_) :: Var(_) :: Nil) => false
          case Var(_) => false
          case _ => true
        }
      }
      if (generatingFunctions.nonEmpty) {
        outputFile.println(verificationGeneration(generatingFunctions, theory.theory))
      }
    }
    outputFile.close()
  }

  def verificationGeneration(generatingFunctions: Seq[Expression], theory: String): String = {
    s"""
       |    ('$theory', ultimate_question.$theory),
     """.stripMargin
  }

  def rename(ex: Expression): Expression = {
    val currentRenaming = collection.mutable.Map[String,String]()
    val stack: collection.mutable.ListBuffer[Unit => Expression] = ListBuffer.empty

    def renameVariables(e: Expression): Expression = e match {
      case Var(name) if currentRenaming.isDefinedAt(name) => Var(currentRenaming(name))
      case Var(name) =>
        val newName = "x"
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

    val result = renameVariables(ex)
    if (currentRenaming.size == 1) result
    else ex
  }

  def main(args: Array[String]): Unit = {
    //    println(FormulaParserInst.parse("[0, 0, 840, 12477384, 2545607472, 116307115440, 2406303387000, " +
    //      "30037635498360, 262918567435104, 1765904422135392, 9653659287290280, 44745048366882600, 181129909217550480, 654743996230865424, 2149893215016113112]"))
    generateForAll()
    //    println(DocumentParser.getGeneratingFunction(TextParserIns.parseLine("Conjecture: G.f.: 1 = Sum_{n>=0} a(n+1)" +
    //      "*A000108(n)*x^n*Sum_{k>=0} C" +
    //      "(2*n+k,k)^2*(-x)^k. Compare with the following g.f of the Catalan numbers (A000108): 1 = Sum_{n>=0} A000108(n)" +
    //      "*x^n*Sum_{k>=0} C(2*n+k,k)*(-x)^k. - _Paul D. Hanna_, Oct 10 2010").get))
    //    TheoryRepDao.findOneByTheory(2207).foreach { x =>
    //      x.formulas.foreach(y => println(y.toString))
    //    }
  }

  def generateForAll() = {
    val outputFile = File("generatingFunctions").printWriter()
    TheoryRepDao.findAll().toList.foreach { theory =>
      val generatingFunctions = theory.generatingFunctions.map(rename).filter { generatingFunction =>
        val simplified = SageWrapper.simplifyFull(generatingFunction)
        simplified.exists {
          case Num(constant) => false
          case Mul(Num(_) :: Var(_) :: Nil) => false
          case Var(_) => false
          case _ => true
        }
      }.filter { generatingFunction =>
        val isValid =
          SageWrapper.oeisOffsets(theory.theory).flatMap { offsets =>
            SageWrapper.oeisTerms(theory.theory).flatMap { originalSequence =>
              val intOffset = offsets.args.head match {
                case Neg(Num(s)) => -s.toInt
                case Num(s) => s.toInt
              }
              SageWrapper.generatingFunctionSeriesCoefficients(
                generatingFunction,
                (if(intOffset<0) 0 else intOffset) + 8
              ).map {
                sequence =>
                val (newSequence, newOriginalSequence) =
                  if (intOffset < 0) {
                    sequence.args -> originalSequence.args.drop(-intOffset)
                  } else {
                    sequence.args.drop(intOffset) -> originalSequence.args
                  }
                sequenceLogger.debug("NewSeq " + newSequence)
                sequenceLogger.debug("NewOrigSeq " + newOriginalSequence)
                if (newSequence == Nil || newOriginalSequence == Nil) false
                else {
                  if (newOriginalSequence.length < newSequence.length) {
                    newSequence.containsSlice(newOriginalSequence)
                  } else {
                    newOriginalSequence.containsSlice(newSequence)
                  }
                }
              }
            }
          }.getOrElse(false)

        if (!isValid) {
          logger.debug(s"${theory.theory}, ${generatingFunction.toSage}")
        }
        isValid
      }
      if (generatingFunctions.nonEmpty) {
        TheoryRepVerifiedDao.save(theory.copy(formulas = Nil, generatingFunctions = generatingFunctions))
        outputFile.println(functionGeneration(generatingFunctions, theory.theory))
        outputFile.println()
      }
    }
    outputFile.close()
  }

  def functionGeneration(generatingFunctions: Seq[Expression], theory: String): String = {
    s"""
       |def $theory():
       |    x = SR.var('x')
       |    return { 'ogf': [${generatingFunctions.map(_.toSagePython).mkString(",\n")}] }
    """.stripMargin
  }

  private def getFirstPair(sequence: List[Expression]): List[Expression] = {
    sequence.collect {
      case ArgList(firstPair :: _ :: Nil) => firstPair
    }
  }

}
