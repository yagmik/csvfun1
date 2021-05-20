package xpr

import java.io.Reader
import scala.io.Source
import scala.util.{Try, Using}

// изначально читалось всё напрямую из файла, но по причинам,
// описанным в CsvLexer, читается сначала в память, и только потом разбирается
trait SourceReader {
  def getSourceReader(path: String): Try[Reader]

  def getSourceAsString(path: String): Try[String] =
    for {
      reader <- getSourceReader(path)
      result <- Using(reader)(readSourceAsString)
    } yield result

  def readSourceAsString(reader: Reader): String = {
    val sb = new StringBuilder()
    val cbuf = new Array[Char](1024)
    var rc = reader.read(cbuf)
    while (rc != -1) {
      sb ++= new String(cbuf, 0, rc)
      rc = reader.read(cbuf)
    }
    sb.toString()
  }
}

trait ResourceReader extends SourceReader {
  def getSourceReader(resource: String): Try[Reader] =
    Try(Source.fromResource(resource).reader())
}

trait FileReader extends SourceReader {
  def getSourceReader(file: String): Try[Reader] =
    Try(Source.fromFile(file).reader())
}
