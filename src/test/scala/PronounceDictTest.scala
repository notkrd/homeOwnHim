import org.scalatest.FunSuite

class PronounceDictTest extends FunSuite {
  val eh_leaf: pronounciation.PronounceDict = new pronounciation.PronounceDict("eh")
  val eh_dict: pronounciation.PronounceDict = new pronounciation.PronounceDict(Map("EH" -> new pronounciation.PronounceDict("eh")))
  val essay_leaf: pronounciation.PronounceDict = new pronounciation.PronounceDict("essay")
  val essay_p: (String, List[String]) = ("essay", List("EH", "S", "AY"))
  val essay_dict = new pronounciation.PronounceDict(Map("EH" -> new pronounciation.PronounceDict(Map("S" -> new pronounciation.PronounceDict(Map("AY" -> new pronounciation.PronounceDict("essay")))))))
  val small_p_dict = new pronounciation.PronounceDict(Map("EH" -> pronounciation.PronounceDict(Set("eh"), Map("S" -> new pronounciation.PronounceDict(Map("AY" -> new pronounciation.PronounceDict("essay")))))))

  test("PronounceDict.toString") {
    println(small_p_dict)
    assert(eh_leaf.toString == "{ \"eh\" }")
    assert(small_p_dict.toString == "{ EH -> { \"eh\" S -> { AY -> { \"essay\" } } } }")
  }

  test("PronounceDict.words") {
    assert(eh_leaf.words == Map("eh" -> List.empty[String]))
    assert(small_p_dict.words == Map("eh" -> List("EH"),"essay" -> List("EH", "S", "AY")))
  }

  test("PronounceDict.::") {
    assert(essay_p :: eh_dict == small_p_dict)
  }
}
