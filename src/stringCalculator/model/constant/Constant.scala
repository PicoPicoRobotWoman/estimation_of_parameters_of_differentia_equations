package stringCalculator.model.constant

trait Constant {

  def getDesignation: String
  def getValue: Double

  override def equals(obj: Any): Boolean = {
    obj match {
      case str: String => getDesignation == str
      case _ => if (obj == null || getClass != obj.getClass) false
      else getDesignation == obj.asInstanceOf[Constant].getDesignation
    }
  }

}
