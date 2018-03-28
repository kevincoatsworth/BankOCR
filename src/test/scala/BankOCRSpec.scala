import org.scalatest.{MustMatchers, WordSpec}

class BankOCRSpec extends WordSpec with MustMatchers {

  "BankOCR" must {

    "return 4 when provided with '\"   \" +\n\"|_|\" +\n\"  |\"' " in {
      BankOCR.numberChecker("   " +
                            "|_|" +
                            "  |" ) mustEqual "4"
    }

    "return 9 when provided with the appropriate input " in {
      BankOCR.numberChecker(" _ " +
                            "|_|" +
                            " _|" ) mustEqual "9"
    }

    "return 8 when provided with the appropriate input " in {
      BankOCR.numberChecker(" _ " +
                            "|_|" +
                            "|_|" ) mustEqual "8"
    }

    "return 48 when provided with the appropriate input " in {
      BankOCR.numberChecker("    _ " +
                            "|_||_|" +
                            "  ||_|" ) mustEqual "48"
    }

    "return 12 when provided with the appropriate input " in {
      BankOCR.numberChecker("    _ " +
                            "  | _|" +
                            "  ||_ " ) mustEqual "12"
    }

    "return 123456789 when provided with the appropriate input" in {
      BankOCR.numberChecker("    _  _     _  _  _  _  _ " +
                            "  | _| _||_||_ |_   ||_||_|" +
                            "  ||_  _|  | _||_|  ||_| _|" ) mustEqual "123456789"
    }

    "return true if 9 digit account number is mod of 11" in {

      BankOCR.checkSum("3,4,5,8,8,2,8,6,5") mustEqual true

    }

    "return false if 9 digit account number is not mod of 11" in {

      BankOCR
      .checkSum("3, 4, 5, 0, 8, 2, 8, 6, 5") mustEqual false
    }

    }
}