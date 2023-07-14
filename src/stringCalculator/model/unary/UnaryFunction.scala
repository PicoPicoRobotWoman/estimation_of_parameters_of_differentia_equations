package stringCalculator.model.unary

trait UnaryFunction {

  def getDesignation: String

  def calc(number: Double): Double

  override def equals(obj: Any): Boolean = {
    obj match {
      case str: String => getDesignation == str
      case _ => if (obj == null || getClass != obj.getClass) false
      else getDesignation == obj.asInstanceOf[UnaryFunction].getDesignation
    }
  }

}
