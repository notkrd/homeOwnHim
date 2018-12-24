package pronounciation

import scala.io.Source

object ReedHing {
  def read_dict(dict_path: String): Seq[String] = {
    var dict_lines = Seq.empty[String]
    val dict_src = Source.fromFile(dict_path)
    try {
      for (line <- dict_src.getLines) {
        dict_lines = line :: dict_lines
      }
    }
    finally {
      dict_src.close()
      dict_lines
    }
  }
}
