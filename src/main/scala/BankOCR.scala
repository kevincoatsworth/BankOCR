object BankOCR extends App {

  def numberChecker(string: String): Int = {

    val numMap = Map (
      " _ " +
      "| |" +
      "|_|" -> 0,
      "   " +
      "  |" +
      "  |" -> 1,
      " _ " +
      " _|" +
      "|_ " -> 2,
      " _ " +
      " _|" +
      " _|" -> 3,
      "   " +
      "|_|" +
      "  |" -> 4,
      " _ " +
      "|_ " +
      " _|" -> 5,
      " _ " +
      "|_ " +
      "|_|" -> 6,
      " _ " +
      "  |" +
      "  |" -> 7,
      " _ " +
      "|_|" +
      "|_|" -> 8,
      " _ " +
      "|_|" +
      " _|" -> 9
    )

    println(string.length)

    val numbers = string.length match {

      case 9 => 1
      case 18 => 2
      case 27 => 3
      case 36 => 4
      case 45 => 5
      case 54 => 6
      case 63 => 7
      case 72 => 8
      case 81 => 9
      case _ => throw new IllegalArgumentException
    }
    println(numbers)

    val reg = ".{3}".r
    val split = reg.findAllIn(string).toList
    val stringList = split.grouped(numbers).toList.transpose
    val strings = stringList.map(s => s.mkString)
    val check = strings.map(s => numMap(s)).mkString.toInt

    check
  }
  numberChecker("    _ "+
                "  | _|"+
                "  ||_ ")
}








//def isMultipleOfNine(int: Int): Boolean = int % 9 == 0

//Make list of multiples of 9
//    val multiples = List(1 to 90 by 9)
//
//    val numbers = if (multiples.contains(string.length)) {
//      string.length
//    } else {
//      throw new IllegalArgumentException
//    }
//    val reg = ".{3}".r
//    val split = reg.findAllIn(string).toList
//    val stringList = split.grouped(numbers).toList.transpose
//    val strings = stringList.map(s => s.mkString)
//    val check = strings.map(s => numMap(s)).mkString.toInt
//
//    println(check)
//    check
