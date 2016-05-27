package parsers
package blast

/** Parse Phytozome files (currently compatible with BLASTP 2.2.26+ only). */
object PhytozomeBlastParser extends BlastParserCompanion {
  def unapply(arg: FileAndContents): Option[BlastParser] = arg match {
    case (file, contents) if file.getName.endsWith("_blast.txt") &&
      (contents.startsWith("BLAST results") || contents.startsWith("Info ?")) =>
        Some(new PhytozomeBlastParser_2_2_26)
    case _  => None
  }
}

/** Parse Phytozome BLASTP 2.2.26+ files. */
class PhytozomeBlastParser_2_2_26 extends BlastParser {
  val numeric = "\\d+([.][0-9]+)?([eE]-?[0-9]+)?" // 10.1E-5
  val numeric3 = numeric.replace("d+", "d{1,3}")
  val decimal = numeric.replaceFirst("[?]", "").r
  val number = numeric.r
  val numberRange = s"$numeric-$numeric(?=\\s|\\z)".r // 123-456

  def blast(logger: Logger): Parser[ExcelCSV] = {
      (wordsUntil("BLAST") ~> "results" ~> query ~ target) ~
      (literal("Program: BLASTP 2.2.26+") ~> literal("revise this query") ~> count) ~
      (literal("Defline \tScore \tE") ~> literal("query sequence") ~> word ~> rep(result)) ~
      opt(".+2006-2013 University of California Regents. All rights reserved".r)
    } ^^ {
      case query ~ target ~ count ~ results ~ footer =>
        if (count != results.size) logger += "***INCONSISTENT COUNT***"
        else if (!footer.isDefined) logger += "***TRUNCATED***"
        results.map(r => query +: results.length.toString +: r)
    }

  def nonNumericNonCopyright = word.filter(w => !parse(phrase(decimal), w).successful) // words other than numbers
  def excel(str: Any) = "=\""+str.toString+"\""
  def query = "Query:" ~> rep(word.filter(_ != "Target:")) ^^ { case words => words.mkString(" ") }
  def target = "Target:" ~> rep(word.filter(_ != "Program:")) ^^ { case words => words.mkString(" ") }
  def count = word <~ literal("genes identified:") ^^ { case word => word.toInt }
  def result = rep1(word, nonNumericNonCopyright) ~ number ~ number ~ rep1(numberRange | numeric3) ^^ {
    case words ~ score ~ eval ~ seqs => Seq(
      words.mkString(" "),
      (score),
      (eval),
      excel(seqs.mkString(","))
    )
  }
} // end of PhytozomeBlastParser_2_2_26

