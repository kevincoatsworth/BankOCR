import scala.util.{Failure, Success, Try}

object BankOCR extends App {

  def numberChecker(string: String): List[String] = {

    val numMap = Map(
      " _ " +
      "| |" +
      "|_|" -> "0",
      "   " +
      "  |" +
      "  |" -> "1",
      " _ " +
      " _|" +
      "|_ " -> "2",
      " _ " +
      " _|" +
      " _|" -> "3",
      "   " +
      "|_|" +
      "  |" -> "4",
      " _ " +
      "|_ " +
      " _|" -> "5",
      " _ " +
      "|_ " +
      "|_|" -> "6",
      " _ " +
      "  |" +
      "  |" -> "7",
      " _ " +
      "|_|" +
      "|_|" -> "8",
      " _ " +
      "|_|" +
      " _|" -> "9"
    )

    val numbers = string
                  .length match {
      case x if x % 9 == 0 => string
                              .length / 9
      case _ => throw new IllegalArgumentException
    }

    val regex = ".{3}".r

    val split = regex.findAllIn(string).toList

    val stringList = split.grouped(numbers).toList.transpose

    val strings = stringList.map(s => s.mkString)

    val check = strings.map(s => {
      Try(numMap(s)) match {
        case Success(c) => c
        case Failure(_) => "?"
      }
    })
    println(check)
    check
  }

  def checkSum(resultOfChecker : List[String]): String = {

    if (resultOfChecker.contains("?")) {
      resultOfChecker.mkString.concat(" ILL")
    } else {

      val reverseNumber = resultOfChecker
                          .map(x => x
                                    .toInt)
                          .reverse
      val reverseListWithIndex = reverseNumber
                                 .zipWithIndex
                                 .map(x => x._1 * (x._2 + 1))
                                 .sum
      val mod = reverseListWithIndex % 11 == 0

      mod match {
        case true => resultOfChecker
                     .mkString
        case false => resultOfChecker
                      .mkString
                      .concat(" ERR")
      }
    }
  }
  def start(){

    val numCheck = numberChecker(" _     _  _  _  _  _  _  _ " +
                                 " _||_||_ |_||_| _||_||_ |_ " +
                                 " _|  | _||_||_||_ |_||_? _|")

    checkSum(numCheck)
  }
  start()
}

