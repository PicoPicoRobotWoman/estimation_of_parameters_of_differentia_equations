package stringCalculator.model.constant

import scala.collection.mutable.ListBuffer

trait ConstantFabric {

  val constantPool: ListBuffer[Constant] = new ListBuffer

  def createConstant(designation: String, value: Double): Constant = {

    if (constantPool.contains(designation)) {
      constantPool.filter(_.getDesignation == designation).head
    } else {
      new Constant {

        override def getDesignation: String = designation

        override def getValue: Double = value
      }
    }

  }

}
