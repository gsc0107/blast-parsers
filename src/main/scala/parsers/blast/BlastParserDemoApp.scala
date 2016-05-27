package parsers
package blast

import java.io.File

import scala.util.{Try,Success => TrySuccess,Failure => TryFailure}

/**
 * Parse some [[demoFiles]] using [[BlastParser]]s for the NCBI BLAST
 * file format. For usage see [[main]] (defaults to "blasts" folder).
 * @see https://en.wikipedia.org/wiki/BLAST
 */
object BlastParserDemoApp {

  type ParserDetectors = PartialFunction[FileAndContents,Option[BlastParser]]

  type DemoResult = (Option[ExcelCSV],Log)

  /** `blasts/` */
  val defaultDir = new File("blasts/")

  /** Usage: `BlastParserDemoApp [-nolegacy] [dir]` */
  def main(args: Array[String]): Unit = args match {
    case Array("-nolegacy") => demo(defaultDir, runBlastFileParser(currentParsers))
    case Array("-nolegacy", dir) => demo(new File(dir), runBlastFileParser(currentParsers))
    case Array(dir) => demo(new File(dir), runBlastFileParser(defaultParsers))
    case _ => demo(defaultDir, runBlastFileParser(defaultParsers))
  }

  def demo(dir: File, runParser: File => DemoResult): Unit = {
    // find all BLAST files to parse
    val files = demoFiles(dir)

    // parse in parallel
    val done = files.par.map(runParser)

    // CSV interpretation: data count is simply numRows
    def csvDataCount(csvOpt: Option[ExcelCSV]) = csvOpt.map(_.size) getOrElse 0

    // aggregate statistics
    val (numResults, numLogs) = done.aggregate((0,0))(
      {case ((nr, nl), (csvOpt, log)) => (nr + csvDataCount(csvOpt), nl + log.size)},
      {case ((nr1, nl1), (nr2, nl2)) => (nr1 + nr2, nl1 + nl2)})

    // output
    println(s"${files.size} files to parse")
    println(s"${done.size} files parsed")
    println(s"$numResults results found")
    println(s"$numLogs log messages")
  }

  /** Demo files, for example: `blasts/speciesN_outputs/resultN_blast.txt`. */
  def demoFiles(dir: File) = for {
    d <- dir.listFiles
    if d.isDirectory && d.getName.endsWith("_outputs")
    f <- d.listFiles
    if f.isFile && f.getName.endsWith(".txt") && !f.getName.startsWith(".")
  } yield f

  /* Demo parsers */

  val currentParsers: ParserDetectors = {
    case NCBIBlastParser(parser) => Some(parser)
  }

  val legacyParsers: ParserDetectors = {
    case PhytozomeBlastParser(parser) => Some(parser)
    case CMerolaeBlastParser(parser) => Some(parser)
  }

  /** Attempt to parse current then legancy formats by default. */
  val defaultParsers: ParserDetectors = currentParsers orElse legacyParsers

  /**
   * Parse one file and return the ExcelCSV (if possible) and log
   * messages.
   */
  def runBlastFileParser(parserDetectors: ParserDetectors = defaultParsers)(infile: File): DemoResult =
    runFileReader(infile).flatMap(fileContents => Try {
      val parser = parserDetectors.applyOrElse((infile, fileContents), (_: FileAndContents) => None)
      val (csv, log) = parser match {
        case Some(parser) =>
          parse(parser, fileContents)
        case None =>
          val preview = fileContents.replaceAll("\n", " ").take(20)
          (None, Seq(infile.getName, s"${infile.length} bytes starting with: $preview"))
      }
      val summary =
        if (parser.isEmpty) "**NO PARSER DETECTED**"
        else if (csv.isEmpty) "**NO GENES IDENTIFIED**"
        else s"Parsed ${csv.size} results"
      (csv getOrElse Seq(Seq(infile.getName, "0")), log :+ summary)
    }) match {
      case TrySuccess((csv, log)) =>
        (Some(csv), log)
      case TryFailure(e) =>
        import scala.collection.JavaConversions._
        (None, "${infile.getName} **UNHANDLED ERROR**" +: e.getStackTrace.map(_.toString))
    }

  /** Convert parser Success/Failure to Some/None. */
  def parse(parser: BlastParser, fileContents: String): DemoResult =
    parser.parse(fileContents) match {
      case BlastParseResult(s: Success, log) => (if (s.result.isEmpty) None else Some(s.result), log)
      case BlastParseResult(x, log) => (None, log :+ x.toString)
    }

  /** File I/O. */
  def runFileReader(infile: File): Try[String] = {
    val reader = scala.io.Source.fromFile(infile, "windows-1252")
    try {
      TrySuccess(reader.getLines.mkString("\n").trim)
    }
    catch {
      case e: Exception => TryFailure(e)
      }
    finally {
      reader.close
    }
  }

} // end of BlastParserDemo

