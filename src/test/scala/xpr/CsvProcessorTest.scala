package xpr

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class CsvProcessorTest extends AnyFunSuite {
  val obj = new CsvProcessor with ResourceReader
  val headless = obj.processSource(false, _)
  val headed = obj.processSource(true, _)

  test("positive: simple.csv") {
    checkPositive("xpr/simple.csv", 3, 2)
  }

  test("positive: simple_crlf.csv") {
    checkPositive("xpr/simple_crlf.csv", 3, 2)
  }

  test("positive: comma_in_quotes.csv") {
    checkPositive("xpr/comma_in_quotes.csv", 5, 2)
  }

  test("positive: empty.csv") {
    checkPositive("xpr/empty.csv", 3, 3)
  }

  test("positive: empty_crlf.csv") {
    checkPositive("xpr/empty_crlf.csv", 3, 3)
  }

  test("positive: escaped_quotes.csv") {
    checkPositive("xpr/escaped_quotes.csv", 2, 3)
  }

  test("positive: json.csv") {
    checkPositive("xpr/json.csv", 2, 2)
  }

  test("positive: newlines.csv") {
    checkPositive("xpr/newlines.csv", 3, 4)
  }

  test("positive: newlines_crlf.csv") {
    checkPositive("xpr/newlines_crlf.csv", 3, 4)
  }

  test("positive: quotes_and_newlines.csv") {
    checkPositive("xpr/quotes_and_newlines.csv", 2, 3)
  }

  test("negative: utf8.csv (illegal character)") {
    checkNegative("xpr/utf8.csv")
  }

  test("negative: simple_diff_size.csv (row size deviation)") {
    checkNegative("xpr/simple_diff_size.csv")
  }

  test("negative: simple_bad_nonescaped.csv (quote in non-escaped field)") {
    checkNegative("xpr/simple_bad_nonescaped.csv")
  }

  // infrastructure
  def checkPositive(resource: String, cols: Int, rows: Int): Unit =
    assert(
      checkPositiveResult(headless, resource, cols, rows) &&
        checkPositiveResult(headed, resource, cols, rows)
    )

  def checkPositiveResult(p: String => Try[File], resource: String, cols: Int, rows: Int): Boolean = p(resource) match {
    case Success(file) =>
      checkFile(file, cols, rows)
    case Failure(_) => false
  }

  def checkFile(file: File, cols: Int, rows: Int): Boolean = file match {
    case HeadlessFile(records) =>
      records.size == rows &&
        records.head.fields.size == cols
    case HeadedFile(records, header) =>
      records.size == rows - 1 &&
        records.head.fields.size == cols &&
        header.names.size == cols &&
        records.head.fields.size == header.names.size
  }

  // TODO как-то надо бы различать разные CsvLexerException. по msg.contains?
  def checkNegative(resource: String): Unit = {
    val result = headless(resource) match {
      case Success(_) => false
      case Failure(exception) =>
        exception match {
          case _: CsvLexerException =>
            true
          case _ => false
        }
    }
    assert(result)
  }
}
