import org.scalatest.{MustMatchers, WordSpec}

class BankOCRSpec extends WordSpec with MustMatchers {

  "BankOCR" must {

    "return 4 when provided with '\"   \" +\n\"|_|\" +\n\"  |\"' " in {
      BankOCR
      .numberChecker("   " +
                     "|_|" +
                     "  |") mustEqual List("4")
    }

    "return 9 when provided with the appropriate input " in {
      BankOCR
      .numberChecker(" _ " +
                     "|_|" +
                     " _|") mustEqual List("9")
    }

    "return 8 when provided with the appropriate input " in {
      BankOCR
      .numberChecker(" _ " +
                     "|_|" +
                     "|_|") mustEqual List("8")
    }

    "return 48 when provided with the appropriate input " in {
      BankOCR
      .numberChecker("    _ " +
                     "|_||_|" +
                     "  ||_|") mustEqual List("4", "8")
    }

    "return 12 when provided with the appropriate input " in {
      BankOCR
      .numberChecker("    _ " +
                     "  | _|" +
                     "  ||_ ") mustEqual List("1", "2")
    }

    "return 123456789 when provided with the appropriate input" in {
      BankOCR
      .numberChecker("    _  _     _  _  _  _  _ " +
                     "  | _| _||_||_ |_   ||_||_|" +
                     "  ||_  _|  | _||_|  ||_| _|") mustEqual List("1", "2", "3", "4", "5", "6", "7", "8", "9")
    }

    "throw new illegalArgumentException if string is not correct format" in {

      val num = "    _  _     _  _  _  _  _ " +
                "  | _| _||_||_ |_   ||_||_|" +
                "  ||_    | _||_|  ||_| _|"
      intercept[IllegalArgumentException] {
        BankOCR
        .numberChecker(num)
      }

      "return list if 9 digit account number is modulos of 11" in {
        BankOCR
        .checkSum(List("3", "4", "5", "8", "8", "2", "8", "6", "5")) mustEqual "345882865"

      }

      "return list with ILL indicator if account number has illegible characters" in {
        BankOCR
        .checkSum(List("3", "4", "5", "8", "8", "2", "8", "?", "5")) mustEqual "3458828?5 ILL"
      }

      "return list with ERR indicator if 9 digit account number not modulos of 11" in {
        BankOCR
        .checkSum(List("3", "4", "5", "8", "8", "2", "8", "6", "6")) mustEqual "345882866 ERR"

      }
    }
}