import org.scalatest.FunSuite

object ReedHingTest extends FunSuite {
  val a_line: String = "EHRENKRANTZ  EH1 R AH0 N K R AE2 N T S"
  val first_line: String = "!EXCLAMATION-POINT  EH2 K S K L AH0 M EY1 SH AH0 N P OY2 N T"
  val some_ehs: String = """|EGYPTIANS  IH0 JH IH1 P SH AH0 N Z
                            |EGYPTOLOGY  IY2 JH AH0 P T AA1 L AH0 JH IY0
                            |EH  EH1
                            |EHINGER  EH1 HH IH0 N JH ER0
                            |EHLE  EH1 L
                            |EHLEN  EH1 L AH0 N
                            |EHLER  EH1 L ER0
                            |EHLERS  EH1 L ER0 Z
                            |EHLERT  EH1 L ER0 T
                            |EHLINGER  EH1 L IH0 NG ER0
                            |EHLKE  EH1 L K"""
}
