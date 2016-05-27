package parsers
package blast

import scala.util.parsing.combinator._

/**
 * Base trait to implement parsers for specific BLAST (or vendor) file
 * formats, returning rows of strings in an Excel-compatible CSV format.
 * Only the file syntax is validated during parsing (content semantics
 * are not guaranteed: use a BLAST interpreter). Implementations
 * provide `blast`, and callers can use the convenience method `parse`.
 * Implementing a BLAST parser:
 * {{{
 * import scala.util.parsing.combinator._
 * class MyBlastParser extends BlastParser {
 *   def blast(log: Logger): Parser[ExcelCSV] = ???
 * }
 * object MyBlastParser extends BlastParserCompanion {
 *   def unapply(arg: FileAndContents): Option[BlastParser] = arg match {
 *     case (file, contents) if ... => Some(new MyBlastParser)
 *     case _ => None
 *   }
 * }
 * }}}
 * Usage:
 * {{{
 * // forced attempt to use BlastParser
 * val parser = new MyBlastParser()
 * val BlastParseResult(status, log) = parser.parse(fileContents)
 * status match { case s: Success => println(s.result); println(log)
 *   case f: Failure => case e: Error => }
 *
 * // conditional attempt using BlastParserCompanion
 * val result: Option[BlastParseResult] = ((file, fileContents) match {
 *   case MyBlastParser(capable) if ... => Some(capable)
 *   case _ => None
 * }) flatMap (_ parse fileContents)
 * }}}
 * @see [[scala.util.parsing.combinator.RegexParsers]]
 * @see https://en.wikipedia.org/wiki/BLAST
 */
trait BlastParser extends RegexParsers {

  /** BLAST word must start with 7-bit ASCII (ignore control and copyright characters). */
  val word = "[ -~]\\S*".r

  /**
   * Attempt to parse the contents of a BLAST file and return a BlastParseResult.
   */
  def parse(fileContents: String): BlastParseResult = {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val result = super.parse(blast(log), fileContents)
    BlastParseResult(result, log)
  }

  /**
   * Internal Parser implementation (Scala RegexParsers).
   * @param log a mutable log buffer to receive messages that will be
   * returned alongside the Scala Parser result.
   */
  def blast(log: Logger): Parser[ExcelCSV]

  /** Word search: return all words until (excluding) the given terminator. */
  def wordsUntil(end: String) = rep(word.filter(_ != end)) ~ end ^^ {case seq ~ end => seq}
} // end of BlastParser

/** See BlastParser for usage. */
trait BlastParserCompanion {

  /**
   * Pattern matching: return a BlastParser implementation
   * corresponding to the format-markers of this File/Contents.
   */
  def unapply(arg: FileAndContents): Option[BlastParser]

} // end of BlastParserCompanion


