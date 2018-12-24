package pronounciation

sealed trait PronounceDict {

  val words: Pronounciations = this match {
    case PLeaf(wrd) => Map(wrd -> List.empty[String])
    case PNode(ph, ents) =>
      ents.foldLeft(Map.empty[String, List[String]])((x: Pronounciations, y: PronounceDict) => x ++ y.words).mapValues(l => ph :: l)
    case PRoot(nds) => nds.foldLeft(Map.empty[String, List[String]])((m, n) => m ++ n.words)
  }

  def toPRoot: PRoot = this match {
    case PLeaf(wrd) => throw new IllegalArgumentException("Attempted to add a word with no noise alongside it, again saturating, dangerously, silence")
    case PNode(ph, ents) => PRoot(Set(PNode(ph, ents)))
    case PRoot(nds) => PRoot(nds)

  }

  def toPNode: PNode = this match {
    case PLeaf(wrd) => new PNode(PLeaf(wrd))
    case PNode(_,_) => this.asInstanceOf[PNode]
    case PRoot(nds) => PNode("", nds.asInstanceOf[Set[PronounceDict]])
  }

  def ::(pronounce: (String, Pronounce)): PronounceDict = pronounce._2 match {
    case Nil => this match {
      case PLeaf(wrd) => PNode("", Set[PronounceDict](this, PLeaf(pronounce._1)))
      case PNode(ph, ents) => PNode(ph, ents + PLeaf(pronounce._1))
      case PRoot(ns) => throw new IllegalArgumentException("Attempted to add a word that makes no noise, overloading silence, filling meaning with an endless, hopeless, hum")
    }
    case fst :: rest => this match {
      case PLeaf(wrd) => PNode("", Set[PronounceDict](PLeaf(wrd), pronounce._1 -> rest :: PNode(fst, Set.empty[PronounceDict])))
      case PNode(ph, ents) => {
        ents foreach {
          case PNode(ph2, ents2) => if(ph2 == fst) {
            return PNode(ph, ents - PNode(ph2, ents2) + (pronounce._1 -> rest :: PNode(ph2, ents2)))
          }
          case _ => Unit
        }
        PNode(ph, ents + ((pronounce._1, rest) :: PNode(fst, Set.empty[PronounceDict])))
      }
      case PRoot(nds) => {
        nds foreach { pn =>
          if(pn.phoneme == fst) {
            return PRoot(nds - PNode(pn.phoneme, pn.entries) + (pronounce._1 -> rest :: PNode(pn.phoneme, pn.entries)).toPNode)
          }
        }
        PRoot(nds + ((pronounce._1, rest) :: PNode(fst, Set.empty[PronounceDict])).toPNode)
      }
    }
  }

  def ++ (that: PronounceDict): PronounceDict = PronounceDict.from_pronounciations(this.words ++ that.words)

  override def toString: String = this match {
    case PLeaf(w) => "\"" ++ w ++ "\""
    case PNode(ph, ents) => ents.foldLeft(s"{ $ph -> ")((s, pd) => s"$s${pd.toString} ") ++ "}"
    case PRoot(nds) => nds.foldLeft("<")(_ + _.toString) + ">"
  }
}

final case class PLeaf(word: String) extends PronounceDict
final case class PNode(phoneme: String, entries: Set[PronounceDict]) extends PronounceDict {
  def this(leaf: PLeaf) = this("", Set(leaf))
  def this() = this("", Set.empty[PronounceDict])
}
final case class PRoot(nodes: Set[PNode]) extends PronounceDict

object PronounceDict {
  def from_pronounciations(prns: Pronounciations): PronounceDict = {
    prns.keysIterator.foldLeft[PronounceDict](new PNode())((n, k) => (k, prns(k)) :: n)
  }

  def empty(): PronounceDict = {
    new PNode()
  }
}