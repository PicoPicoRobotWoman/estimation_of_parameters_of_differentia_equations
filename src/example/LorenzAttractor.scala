package example

import stringCalculator.StringCalculator
import stringCalculator.model.binary.BinaryOperationFabric
import stringCalculator.model.constant.ConstantFabric
import systemParameterEstimator.SystemParameterEstimator

import scala.util.Random

object LorenzAttractor extends App with StringCalculator with ConstantFabric with BinaryOperationFabric with SystemParameterEstimator {

  addBinaryOperation(createBinary("+", 1, (left, right) => left + right, 0))
  addBinaryOperation(createBinary("-", 1, (left, right) => left - right, 0))
  addBinaryOperation(createBinary("*", 2, (left, right) => left * right, 1))
  addBinaryOperation(createBinary("/", 2, (left, right) => left / right, 1))

  val rnd = new Random()

  val n = 1000
  val alpha = 1.25
  val h = Math.pow(n, -alpha)

  val bi: List[Double] = List(2, 3, 1)
  val yi0: List[Double] = List(1, 2, 1)
  val systems: List[String] = List("b1*(y2-y1)", "y1*(b2-y3)-y2", "y1*y2-b3*y3")
  val reverseSystems: List[String] = List("F1/(y2-y1)", "(F2+y2)/y1+y3", "(y1*y2 - F3)/y3")

  bi.zipWithIndex.foreach(b_index => addConstant(createConstant(s"b${b_index._2 + 1}", b_index._1)))

  val positivePreciseObservations: List[List[Double]] = (1 to n).foldLeft(List(yi0)) { (acc, itr) =>
    acc:+ systems.zipWithIndex.map { case (fun, index) =>
      acc.last(index) + h * calkFun(fun, acc.last)
    }
  }

  val negativePreciseObservations: List[List[Double]] = (1 to n).foldRight(List(yi0)) { (itr, acc) =>
    systems.zipWithIndex.map { case (fun, index) =>
      acc.head(index) - h * calkFun(fun, acc.head)
    } :: acc
  }.init

  val preciseObservations: List[List[Double]] = (negativePreciseObservations ::: positivePreciseObservations).transpose

  val nonPreciseObservations: List[List[Double]] = preciseObservations.map(nums => {
    nums.map(num => (1 + Math.pow(-1, rnd.nextInt()) * rnd.nextDouble() * 0.2) * num)
  })

  println(estimator(nonPreciseObservations, reverseSystems, h))

  private def calkFun(fun: String, values: List[Double]): Double = {
    calculate(values.indices.foldLeft(fun)((acc, i) => acc.replace(s"y${i + 1}", s"(${values(i)})")))
  }

}
