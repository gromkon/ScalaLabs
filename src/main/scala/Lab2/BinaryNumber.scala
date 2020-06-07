package Lab2

class BinaryNumber (number: String) {
  val num: String = number

  val ZERO = '0'
  val ONE = '1'

  def + (bn: BinaryNumber): BinaryNumber = {
    var plusOne = false
    val sb:StringBuilder = new StringBuilder

    var smallNumber, bigNumber = ""
    if (num.length <= bn.num.length) {
      smallNumber = num
      bigNumber = bn.num
    } else {
      smallNumber = bn.num
      bigNumber = num
    }

    smallNumber = smallNumber.reverse
    bigNumber = bigNumber.reverse

    for (i <- smallNumber.indices) {
      if (smallNumber(i).equals(ONE) && bigNumber(i).equals(ONE)) {
        if (plusOne) {
          sb.append(ONE)
        } else {
          sb.append(ZERO)
        }
        plusOne = true
      } else if (smallNumber(i).equals(ONE) || bigNumber(i).equals(ONE)) {
        if (plusOne) {
          sb.append(ZERO)
          plusOne = true
        } else {
          sb.append(ONE)
          plusOne = false
        }
      } else {
        if (plusOne) {
          sb.append(ONE)
          plusOne = false
        } else {
          sb.append(ZERO)
          plusOne = false
        }
      }
    }

    for (i <- smallNumber.length until bigNumber.length) {
      if (bigNumber(i).equals(ONE)) {
        if (plusOne) {
          sb.append(ZERO)
          plusOne = true
        } else {
          sb.append(ONE)
          plusOne = false
        }
      } else {
        if (plusOne) {
          sb.append(ONE)
          plusOne = false
        } else {
          sb.append(ZERO)
          plusOne = false
        }
      }
    }

    if (plusOne) {
      sb.append(ONE)
    }

    new BinaryNumber(sb.toString().reverse)
  }

  def * (bn: BinaryNumber): BinaryNumber = {
    val size = num.length + bn.num.length - 1
    val countLines = bn.num.length
    val table = new Array[String](countLines)
    var line = ""
    var emptyLine = ""
    for (_ <- 0 until size) {
      emptyLine = emptyLine.concat("0")
    }
    for (i <- bn.num.indices) {
      if (bn.num(i).equals(ONE)) {
        line = num
        for (_ <- 0 until i) {
          line = line.concat("0")
        }
        for (_ <- line.length until size) {
          line = "0".concat(line)
        }
        table(i) = line
      } else {
        table(i) = emptyLine
      }
    }

    val sb: StringBuilder = new StringBuilder
    var plus = 0
    var sum = 0
    for (i <- 0 until size) {
      sum = 0
      for (j <- table.indices) {
        sum += table(j).reverse(i) - 48
      }
      sum += plus

      plus = 0
      while (sum >= 2) {
        plus = plus + 1
        sum = sum - 2
      }
      sb.append(sum.toString)
    }

    while (plus > 0) {
      if (plus == 1) {
        sb.append(ONE)
        plus = 0
      } else {
        sb.append(ZERO)
        plus = plus - 1
      }
    }

    new BinaryNumber(sb.toString().reverse)
  }

  override def toString: String = {
    num
  }
}

object Lab2 {

  def main(args: Array[String]): Unit = {
    val bn1 = new BinaryNumber("1111010101");
    println("bn1 = " + bn1)
    val bn2 = new BinaryNumber("10101");
    println("bn2 = " + bn1)
    val bn3 = bn1 * bn2
    println("bn1 * bn2 = " + bn3)
    val bn4 = bn1 + bn2
    println("bn1 + bn2 = " + bn4)

    println("-------------------check-------------------")
    println("bn1 in decimal system = " + Integer.parseInt(bn1.num, 2))
    println("bn2 in decimal system = " + Integer.parseInt(bn2.num, 2))
    println("bn1 * bn2 in decimal system = " + Integer.parseInt(bn3.num, 2))
    println("bn1 + bn2 in decimal system = " + Integer.parseInt(bn4.num, 2))
  }
}