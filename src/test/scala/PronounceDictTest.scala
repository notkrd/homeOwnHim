import org.scalatest.FunSuite

class PronounceDictTest extends FunSuite {
  val eh_leaf = pronounciation.PLeaf("eh")
  val eh_node = pronounciation.PNode("EH", Set(eh_leaf))
  val essay_leaf = pronounciation.PLeaf("essay")
  val essay_p: (String, List[String]) = ("essay", List("EH", "S", "AY"))
  val essay_node = pronounciation.PNode("S", Set(pronounciation.PNode("AY", Set(essay_leaf))))
  val ehs_node = pronounciation.PNode("EH", Set(eh_leaf, essay_node))
  val small_p_dict: pronounciation.PronounceDict = pronounciation.PRoot(Set(ehs_node))

  test("PronounceDict.toString") {
    print(small_p_dict)
    assert(eh_leaf.toString == "\"eh\"")
    assert(ehs_node.toString == "{ EH -> \"eh\" { S -> { AY -> \"essay\" } } }")
  }

  test("PronounceDict.words") {
    assert(eh_leaf.words == Map("eh" -> List.empty[String]))
    assert(ehs_node.words == Map("eh" -> List("EH"),"essay" -> List("EH", "S", "AY")))
  }

  test("PronounceDict.::") {
    assert(essay_p :: eh_node.toPRoot == small_p_dict)
  }
}
