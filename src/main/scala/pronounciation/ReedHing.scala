package pronounciation

import scala.io.Source

object ReedHing {
  def readDict(dict_path: String): List[String] = {
    var dict_lines = List.empty[String]
    val dict_src = Source.fromFile(dict_path)
    try {
      dict_src.getLines.foldRight(List.empty[String])(_ :: _)
    }
    catch {
      case e: Exception =>
        println(s"Error reading file $dict_path with error $e")
        List.empty[String]
    }
    finally {
      dict_src.close()
    }
  }

  def processLine(a_line: String): Option[Pronounciation] = {
    val split_line: List[String] = a_line.split(Array(' ')).toList
    split_line match {
      case Nil => None
      case fst :: rst => Some(fst.stripMargin.toLowerCase, rst.filter(_ != ""))
    }
  }

  def fromLines(some_lines: List[String]): PronounceDict = {
    some_lines.foldLeft(PronounceDict.empty)(
      (d, l) => processLine(l) match {
        case None => d
        case Some(p) => p :: d
    })
  }

  def processDict(dict_path: String): PronounceDict = {
    try {
      val the_lines = readDict(dict_path)
      fromLines(the_lines)
    }
    catch {
      case e: Exception =>
        println(s"Failed to read dict at $dict_path with error $e")
        pronounciation.PronounceDict.empty
    }
  }
}
