package xpr

import scala.util.{Try, Failure => TryFailure, Success => TrySuccess}

trait CsvProcessor extends CsvLexer {
  this: SourceReader =>

  def processSource(headed: Boolean, path: String): Try[File] = {
    for {
      string <- getSourceAsString(path)
      parseResult <- process(headed, string)
      file <- postParse(parseResult)
    } yield file
  }

  // ParseResult to Try
  def postParse(pr: ParseResult[File]): Try[File] = pr match {
    case Success(file, _) =>
      val fsdIdx = firstSizeDeviation(file)
      if (fsdIdx == -1) {
        TrySuccess(file)
      } else {
        TryFailure(new CsvLexerException(s"inconsistency encountered - different record size at line $fsdIdx"))
      }
    case e@Error(_, _) =>
      TryFailure(new CsvLexerException(e.toString()))
    case f@Failure(_, _) =>
      TryFailure(new CsvLexerException(f.toString()))
  }

  def firstSizeDeviation(file: File): Int = {
    val (firstRecord, add) =
      file match {
        case HeadlessFile(records) =>
          (records.head.fields, 0)
        case HeadedFile(_, header) =>
          (header.names, 1)
      }
    file.records
      .iterator
      .zipWithIndex
      .find(p => p._1.fields.size != firstRecord.size) match {
      case Some((_, idx)) => idx + add + 1 // first line in file has #1
      case None => -1
    }
  }
}
