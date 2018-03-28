object BankOCR extends App {

  def numMap = Map(
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

  def numberChecker(string: String): String = {

    val numbers = string
                  .length match {
      case x if x % 9 == 0 => string
                              .length / 9
      case _ => throw new IllegalArgumentException

    }

    val regex = ".{3}"
      .r

    val splitCells = regex
                .findAllIn(string)
                .toList

    val splitCellsToList = splitCells
                     .grouped(numbers)
                     .toList
                     .transpose

    val groupedCellStrings: List[String] = splitCellsToList
                                           .map(s => s.mkString)

    val compareWithMap = groupedCellStrings.map(s => numMap(s)).mkString


   println(splitCells)
    println(splitCellsToList)
    println(groupedCellStrings)
    println(compareWithMap)
    compareWithMap

  }

  def checkSum(resultOfChecker : String): Boolean = {

    val reverseListOfNumbers = resultOfChecker.map(_.toInt).reverse
    val reverseListWithIndexSum = reverseListOfNumbers.zipWithIndex.map(x => x._1*(x._2 + 1)).sum
    val mod11remainder = reverseListWithIndexSum % 11 == 0
    println(mod11remainder)
    mod11remainder

  }

  def start(){
    val input = numberChecker(" _     _  _  _  _  _  _  _ " +
                              " _||_||_ |_||_| _||_||_ |_ " +
                              " _|  | _||_||_||_ |_||_| _|")
    checkSum(input)
  }
  start()
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
