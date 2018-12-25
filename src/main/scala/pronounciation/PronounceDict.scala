package pronounciation

import scala.annotation.tailrec

case class PronounceDict(leaves: Set[String], phonemes: Map[String, PronounceDict]) {

  def addLeaf(new_leaf: String): PronounceDict = PronounceDict(leaves + new_leaf, phonemes)

  def updatePhon(a_phon: String)(new_dic: PronounceDict): PronounceDict = PronounceDict(leaves, phonemes.updated(a_phon, new_dic))

  @tailrec
  final def dictAt(a_path: Pronounce): Option[PronounceDict] = a_path match {
    case Nil => Some(this)
    case fst :: rst => if(phonemes.contains(fst)) {
      phonemes(fst) dictAt rst
    }
    else {
      None
    }
  }

  def this(leaf: String) = this(Set(leaf), Map.empty[String, PronounceDict])
  def this(phonemes: Map[String, PronounceDict]) = this(Set.empty[String], phonemes)
  def this() = this(Set.empty[String], Map.empty[String, PronounceDict])

  val words: Pronounciations = {
    val leaf_words: Pronounciations = leaves.map((_, List.empty[String])).toMap
    val branch_words: Pronounciations = phonemes flatMap (spd => {
      spd._2.words mapValues (spd._1 :: _)
    })
    leaf_words ++ branch_words
  }

  def ::(pronounce: (String, Pronounce)): PronounceDict = pronounce._2 match {
    case Nil => PronounceDict(leaves + pronounce._1, phonemes)
    case fst :: rst =>
      PronounceDict(
        leaves,
        phonemes.updated(fst,
          if (phonemes contains fst) {
            (pronounce._1, rst) :: phonemes(fst)
          }
          else {
            (pronounce._1, rst) :: new PronounceDict()
          }))
  }

  def ++ (that: PronounceDict): PronounceDict = PronounceDict(this.leaves ++ that.leaves, this.phonemes ++ that.phonemes)

  override def toString: String = phonemes.foldLeft(leaves.foldLeft("{ ")((s, l) => s ++ "\"" ++ l ++ "\" " ))((s, spd) => s ++ spd._1 ++ " -> " ++ spd._2.toString ++ " ") ++ "}"
}

object PronounceDict {
  def empty: PronounceDict = {
    PronounceDict(Set.empty[String], Map.empty[String, PronounceDict])
  }

  @tailrec
  final def propagateUpdate(new_dict: PronounceDict)(ancestors: List[(String, PronounceDict)]): PronounceDict = ancestors match {
    case Nil => new_dict
    case (s, pd) :: rst =>
      propagateUpdate(pd.updatePhon(s)(new_dict))(rst)
  }

  @tailrec
  private def updateAtHelper(a_dict: PronounceDict)(a_path: Pronounce)(a_val: String)(ancestors: List[(String, PronounceDict)]): PronounceDict = a_path match {
    case Nil => propagateUpdate(a_dict.addLeaf(a_val))(ancestors)
    case fst :: rst =>
      updateAtHelper(a_dict.phonemes(fst))(rst)(a_val)((fst, a_dict) :: ancestors)
  }

  def updateAt(a_dict: PronounceDict)(a_path: Pronounce)(a_val: String): PronounceDict = updateAtHelper(a_dict)(a_path)(a_val)(List.empty[(String, PronounceDict)])



  @tailrec
  private def addKeyHelper(a_dict: PronounceDict)(a_path: Pronounce)(a_key: String)(ancestors: List[(String, PronounceDict)]): PronounceDict = a_path match {
    case Nil => propagateUpdate(a_dict.updatePhon(a_key)(new PronounceDict()))(ancestors)
    case fst :: rst =>
      addKeyHelper(a_dict.phonemes(fst))(rst)(a_key)((fst, a_dict) :: ancestors)
  }

  def addKey(a_dict: PronounceDict)(a_path: Pronounce)(a_val: String): PronounceDict = addKeyHelper(a_dict)(a_path)(a_val)(List.empty[(String, PronounceDict)])

  @tailrec
  final def addPronounce(a_dict: PronounceDict)(pronounce: (String, Pronounce))(curr_path: Pronounce): PronounceDict = a_dict.dictAt(curr_path) match {
    case None => throw new IllegalArgumentException("Invalid current path")
    case Some(d) =>
      pronounce._2 match {
        case Nil => updateAt(a_dict)(curr_path)(pronounce._1)
        case fst :: rst => if (a_dict.phonemes contains fst) {
          addPronounce(a_dict)(pronounce._1, rst)(curr_path ++ List(fst))
        }
        else {
          addPronounce(addKey(a_dict)(curr_path)(fst))(pronounce._1, rst)(fst :: curr_path)
        }
      }
  }

  def from_pronounciations(prns: Pronounciations): PronounceDict = prns.foldRight(PronounceDict.empty)(_ :: _)
}