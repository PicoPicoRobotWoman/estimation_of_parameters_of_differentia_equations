package systemParameterEstimator

import stringCalculator.StringCalculator

trait SystemParameterEstimator extends StringCalculator {

  def estimator(nonPreciseObservations: List[List[Double]], reverseSystems: List[String], h: Double): List[Double] = {

    val n = (nonPreciseObservations.head.size - 1 ) / 2

    val yi0 = nonPreciseObservations.map(yi => yi.sum / yi.size)

    val Fi0 = nonPreciseObservations.map(yi => {
      (-n to n).zip(yi).map(k_y => k_y._1 * k_y._2 * h).sum / (-n to n).map(k => Math.pow(k * h, 2)).sum
    })

    reverseSystems.zipWithIndex.map( sys_index => calkFun(sys_index._1.replace("F", s"${Fi0(sys_index._2)}"), yi0))

  }

  private def calkFun(fun: String, values: List[Double]): Double = {
    calculate(values.indices.foldLeft(fun)((acc, i) => acc.replace(s"y${i + 1}", s"(${values(i)})")))
  }

}
