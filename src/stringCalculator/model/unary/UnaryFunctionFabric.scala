package stringCalculator.model.unary

import scala.collection.mutable.ListBuffer

trait UnaryFunctionFabric {

  private val unaryFunctionsPool: ListBuffer[UnaryFunction] = new ListBuffer

  def createUnary(designation: String, fun: Double => Double): UnaryFunction = {

    if (unaryFunctionsPool.contains(designation)) {
      unaryFunctionsPool.filter(_.getDesignation == designation).head
    }
    new UnaryFunction {
      override def getDesignation: String = designation

      override def calc(number: Double): Double = fun(number)
    }
  }

}
