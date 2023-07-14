package stringCalculator.model.binary

import scala.collection.mutable.ListBuffer

trait BinaryOperationFabric {

  private val binaryOperationPool: ListBuffer[BinaryOperation] = new ListBuffer

  def createBinary(designation: String, priority: Int, fun: (Double, Double) => Double, singleElement: Double): BinaryOperation = {

    if(binaryOperationPool.contains(designation)) {
      binaryOperationPool.filter(_.getDesignation == designation).head
    } else {

      val binary = new BinaryOperation {
        override def getDesignation: String = designation

        override def getPriority: Int = priority

        override def calc(left: Double, right: Double): Double = fun(left, right)

        override def getSingleElement: Double = singleElement
      }

      binaryOperationPool.append(binary)
      binary

    }

  }

}
