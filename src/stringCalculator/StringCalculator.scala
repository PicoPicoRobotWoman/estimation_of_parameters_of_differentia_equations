package stringCalculator

import stringCalculator.model.binary.BinaryOperation
import stringCalculator.model.constant.Constant
import stringCalculator.model.unary.UnaryFunction

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Try

trait StringCalculator {

  private val binaryOperations: ListBuffer[BinaryOperation] = new ListBuffer[BinaryOperation]
  private val unaryFunctions: ListBuffer[UnaryFunction] = new ListBuffer[UnaryFunction]
  private val constants: ListBuffer[Constant] = new ListBuffer[Constant]
  private val separator: String = "†"

  def calculate(expression: String): Double = {

    val replacedExpression = replaceExpressionsWithExclamation(expression.replace(" ", ""))

    val startExpression = binaryOperations.foldLeft(replacedExpression) {
      case (acc, binary) if acc.startsWith(binary.getDesignation) => s"${binary.getSingleElement}$acc"
      case (acc, _) => acc
    }

    val reducedExpression: String = binaryOperations.map(binary => binary.getDesignation)
      .foldLeft(startExpression)((str, designation) => str.replace(designation.toString, s"$separator$designation$separator"))
      .replace("(", s"$separator($separator")
      .replace(")", s"$separator)$separator")
      .replace(s"$separator$separator", separator)

    val tokens: List[String] = reducedExpression.split(separator).toList

    val tokenWithConstant = constants.foldLeft(tokens) { (acc, _) =>
      acc.flatMap { token =>
        constants.find(_.getDesignation == token)
          .map(_.getValue.toString)
          .orElse(Some(token))
      }
    }


    val fin = binaryOperations.foldLeft(tokenWithConstant) { (acc, binary) =>
      val indexing = acc.zipWithIndex.collect {
        case (token, index) if token == binary.getDesignation => index
      }

      indexing.foldRight(acc) { (index, acc) =>
        if (!isNumeric(acc(index - 1))) {
          val result = calculate(s"${binary.getSingleElement}${binary.getDesignation}" + acc(index + 1)).toString
          acc.patch(index - 1, Seq(result), 2)
        } else {
          acc
        }
      }

    }

    val tokensUnWithBinary = fin.reverse.foldLeft(List(tokens.last)) { (acc, token) =>
      val updatedAcc = if (unaryFunctions.contains(token)) {
        val num = acc.last.toDouble
        acc.init :+ unaryFunctions.find(_.getDesignation == token).map(_.calc(num).toString).getOrElse(acc.last)
      } else {
        acc :+ token
      }
      updatedAcc
    }.reverse.init.filterNot(_.isEmpty)

    val minPriority = binaryOperations.map(_.getPriority).min
    implicit val ordering: Ordering[BinaryOperation] = Ordering.by(1.0 / _.getPriority)
    val orderedOperations = binaryOperations.toList.filter(_.getPriority != minPriority).sorted

    val finalTokens = orderedOperations.foldLeft(tokensUnWithBinary) { (acc, binary) =>
      val indexes = acc.zipWithIndex.collect {
        case (token, index) if token == binary.getDesignation => index
      }

      indexes.foldRight(acc) { (index, acc) =>
        val left = acc(index - 1).toDouble
        val right = acc(index + 1).toDouble
        acc.patch(index - 1, Seq(binary.calc(left, right).toString), 3)
      }
    }

    val numbers: List[Double] = finalTokens.filter(token => isNumeric(token)).map(token => token.toDouble)
    val operations: List[String] = finalTokens.filter(token => !isNumeric(token))

    performCalculations(numbers, operations)

  }

  def addBinaryOperation(binary: BinaryOperation): Unit = binaryOperations += binary
  def addUnaryFunction(unary: UnaryFunction): Unit = unaryFunctions += unary
  def addConstant(const: Constant): Unit = constants += const

  @tailrec
  private def performCalculations(numbers: List[Double], operations: List[String]): Double = {

    val right: Double = numbers.last

    if (operations.nonEmpty) {
      operations.last match {
        case x if (binaryOperations.contains(x)) =>
          val res = binaryOperations.filter(op => op.equals(x)).head.calc(numbers(numbers.size - 2), right)
          if (numbers.init.init.isEmpty && operations.init.isEmpty) res else performCalculations(numbers.init.init :+ res, operations.init)
        case token => throw new Exception(s"$token - неизвестный токен")
      }
    } else {
      numbers.last
    }

  }

  private def replaceExpressionsWithExclamation(str: String): String = {

    (separator + "(.*?)" + separator).r.replaceAllIn(str.foldLeft(("", 0)) { (acc, char) =>
      val (output, bracketDepth) = acc
      if (char == '(') (output + (if (bracketDepth > 0) char else separator), bracketDepth + 1)
      else if (char == ')') (output + (if (bracketDepth > 1) char else separator), bracketDepth - 1)
      else (output + char, bracketDepth)
    }._1 , { m =>
      s"$separator${calculate(m.group(1))}$separator"
    })

  }

  private  def isNumeric(token: String): Boolean = {
    Try(token.toDouble).isSuccess
  }

}
