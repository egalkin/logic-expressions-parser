package generator

import java.util.Random

object Test extends App {
  val r = new Random()
  println(TestGenerator.generateTest(10))

}

object TestGenerator {
  val r = new Random()
  val binOps = Array("|", "^", "&")

  def generateTest(opNum: Int): String = {
    val test = StringBuilder.newBuilder
    test.append(getOperand + binOps(r.nextInt(3)) + getOperand)
    for (i <- 1 to opNum) {
      test.append(binOps(r.nextInt(3)) + getOperand)
    }
    test.toString()
  }


  private def getOperand: String = {
    getNegation + getRandomChar
  }

  private def getRandomChar: Char = {
    (r.nextInt(26) + 97).toChar
  }

  private def getNegation: String = {
    if (1 + r.nextInt(100) < 75)
      ""
    else
      "!"
  }
}
