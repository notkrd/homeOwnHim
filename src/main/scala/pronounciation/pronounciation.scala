package object pronounciation {
  type Phoneme = String
  type Pronounce = List[String]
  type Pronounciation = (String, List[String])
  type Pronounciations = Map[String, List[String]]
}
