package parsers
package blast

/** Parse NCBI formats: BLASTP 2.25+, BLASTP 2.2.28+ and TBLASTN 2.2.28+. */
object NCBIBlastParser extends BlastParserCompanion {
  import java.util.regex.Pattern

  val header = Pattern.compile("^(BLASTP|TBLASTN) 2")
  val prefix = Pattern.compile("^(ref|lcl|gb)[|]")

  def unapply(arg: FileAndContents) = arg match {
    case (file, contents) if (
      file.getName.endsWith("-Alignment.txt") ||
      file.getName.endsWith("_output.txt")
    ) && header.matcher(contents).lookingAt =>
      Some(new NCBIBlastParser_2_2_28)
    case _ => None
  }
}

/** Parse NCBI formats: BLASTP 2.25+, BLASTP 2.2.28+ and TBLASTN 2.2.28+. */
class NCBIBlastParser_2_2_28 extends BlastParser {
  import NCBIBlastParser._

  val termination = "(ALIGNMENTS|>lcl)".r // balance E.silic with H.sapiens

  override def parse(fileContents: String): BlastParseResult =
    if (fileContents contains "***** No hits found *****")
      BlastParseResult(Success(Nil, Empty), Nil)
    else super.parse(fileContents)

  def blast(logger: Logger): Parser[ExcelCSV] = {
      (wordsUntil("Query=") ~> word) ~
      (wordsUntil("Value") ~> rep(result)) ~
      opt(termination)
    } ^^ {
      case query ~ results ~ tail =>
        //println(s"$query\n$results")
        if (!tail.isDefined) logger += "***TRUNCATED***"
        results.map(r => query +: results.length.toString +: r)
    }

  def refWord = word.filter(w => prefix.matcher(w).lookingAt)

  def nonRefWord = word.filter(w => !prefix.matcher(w).lookingAt && termination.findPrefixOf(w).isEmpty)

  def trimRef(str: String) = prefix.matcher(str).replaceAll("")

  def result = refWord ~ rep(nonRefWord) ^^ {
    case ref ~ words =>
      val id = trimRef(ref) +: words.dropRight(2)
      Seq(
        id.mkString(" "),
        words.takeRight(2).head,
        words.takeRight(1).head,
        ""
      )
  }
} // end of NCBIBlastParser_2_2_28

