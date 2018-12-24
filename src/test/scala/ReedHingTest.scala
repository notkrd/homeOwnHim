import org.scalatest.FunSuite

class ReedHingTest extends FunSuite {
  val a_line: String = "EHRENKRANTZ  EH1 R AH0 N K R AE2 N T S"
  val a_pronounciation: (String, List[String]) = ("ehrenkrantz", List("EH1", "R", "AH0", "N", "K", "R", "AE2", "N", "T", "S"))
  val line9_pronounciation: (String, List[String]) = ("abominations", List("AH0", "B", "AA2", "M", "AH0", "N", "EY1", "SH", "AH0", "N", "Z"))
  val first_line: String = "!EXCLAMATION-POINT  EH2 K S K L AH0 M EY1 SH AH0 N P OY2 N T"
  val test_line9: String = "ABOMINATIONS  AH0 B AA2 M AH0 N EY1 SH AH0 N Z"
  val some_ehs: String = """|EGYPTIANS  IH0 JH IH1 P SH AH0 N Z
                            |EGYPTOLOGY  IY2 JH AH0 P T AA1 L AH0 JH IY0
                            |EH  EH1
                            |EHINGER  EH1 HH IH0 N JH ER0
                            |EHLE  EH1 L
                            |EHLEN  EH1 L AH0 N
                            |EHLER  EH1 L ER0
                            |EHLERS  EH1 L ER0 Z
                            |EHLERT  EH1 L ER:::0 T
                            |EHLINGER  EH1 L IH0 NG ER0
                            |EHLKE  EH1 L K"""
  val test_dict_file = "test_dict.txt"
  val cmu_dict_file = "pronounce_dict.txt"

  test("ReedHing.readDict") {
    val test_lines = pronounciation.ReedHing.readDict(test_dict_file)
    assert(test_lines.length == 53)
    assert(test_lines(8) == test_line9)
  }

  test("ReedHing.processLine") {
    assert(pronounciation.ReedHing.processLine(a_line).contains(a_pronounciation))
    assert(pronounciation.ReedHing.processLine(test_line9).contains(line9_pronounciation))
  }

  test("ReedHing.fromLines") {
    val test_lines = List("AAA A A A", "BAA B A A")
    val test_dict = pronounciation.ReedHing.fromLines(test_lines)
    val test_words = test_dict.words
    assert(test_words("baa") == List("B", "A", "A"))
  }

  test("ReedHing.processDict") {
    val test_dict = pronounciation.ReedHing.processDict(test_dict_file)
    val test_words = test_dict.words
    assert(test_dict == pronounciation.PronounceDict.from_pronounciations(test_words))
    assert(test_words(line9_pronounciation._1) == line9_pronounciation._2)
//    val full_dict = pronounciation.ReedHing.processDict(cmu_dict_file)
//    val all_words = full_dict.words
//    assert(all_words.keys.size == 133854)
  }
}
