package parsers
package blast

/** Parse legancy BLASTP 2.2.14 from http://merolae.biol.s.u-tokyo.ac.jp */
object CMerolaeBlastParser extends BlastParserCompanion {
  import java.util.regex.Pattern

  val header = Pattern.compile("^# BLASTP 2")
  val nohits = Pattern.compile("[*] No hits found [*]")

  def unapply(arg: FileAndContents) = arg match {
    case (file, contents) if file.getName.endsWith("_blast.txt") &&
      (header.matcher(contents).lookingAt || nohits.matcher(contents).find) =>
        Some(new CMerolaeBlastParser_2_2_14)
    case _ => None
  }
}

/** Parse legancy BLASTP 2.2.14 from http://merolae.biol.s.u-tokyo.ac.jp */
class CMerolaeBlastParser_2_2_14 extends BlastParser {
  import CMerolaeBlastParser._

  override def parse(fileContents: String): BlastParseResult =
    if (nohits.matcher(fileContents).find) BlastParseResult(Success(Nil, Empty), Nil)
    else super.parse(fileContents)

  def blast(logger: Logger): Parser[ExcelCSV] = {
      (wordsUntil("Query:") ~> word) ~
      (wordsUntil("e-value,") ~> wordsUntil("e-value,") ~> word ~> word ~> rep(result))
    } ^^ {
      case query ~ results =>
        results.map(r => query +: results.length.toString +: r)
    }

  def result = word ~ word ~ repN(8, word) ~ word ~ word ^^ {
    case query ~ subject ~ _ ~ eval ~ score => Seq(
      subject.replaceAll("^[^|]*[|][^|]*[|]", ""),
      eval,
      score,
      ""
    )
  }

} // end of CMerolaeBlastParser_2_2_14

