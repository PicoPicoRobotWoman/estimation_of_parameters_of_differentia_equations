package stringCalculator.model.binary

trait BinaryOperation {

  def getDesignation: String
  def getPriority: Int
  def getSingleElement: Double

  def calc(left: Double, right: Double): Double

  override def equals(obj: Any): Boolean = {
    obj match {
      case str: String => getDesignation == str
      case _ => if (obj == null || getClass != obj.getClass) false
      else getDesignation == obj.asInstanceOf[BinaryOperation].getDesignation
    }
  }

}
