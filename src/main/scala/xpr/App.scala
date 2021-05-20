package xpr

import scala.util.{Try, Failure => TryFailure, Success => TrySuccess}

object App extends CsvProcessor with FileReader {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      showUsage()
      System.exit(-2)
    }

    val result =
      for {
        (headed, path) <- Try(parseCommandLine(args))
        file <- processSource(headed, path)
      } yield file

    result match {
      case TrySuccess(file) =>
        showFile(file)
        System.exit(0)
      case TryFailure(exception) =>
        System.err.println(s"Error: ${exception.getMessage}")
        System.exit(-1)
    }
  }

  def showUsage(): Unit = {
    val msg =
      """Usage: {h | hl} filename
        |  h - headed file
        |  hl - headless file
        |""".stripMargin
    System.out.println(msg)
  }

  def parseCommandLine(args: Array[String]): (Boolean, String) = {
    require(args.length == 2, "invalid command line")
    val first = args(0)
    require(Seq("h", "hl").contains(first), "first argument should be {h | hl}")
    (first == "h", args(1))
  }

  def showFile(file: File): Unit = {
    file match {
      case HeadlessFile(_) =>
      case HeadedFile(_, header) =>
        showHeader(header)
    }
    showRecords(file.records)
  }

  def showRecords(records: Iterable[Record]): Unit = {
    println("Records:")
    println(
      records
        .map(r => r.fields.map(f => normalize(f.value)).mkString(", "))
        .mkString(System.lineSeparator())
    )
  }

  def showHeader(header: Header): Unit = {
    println("Header:")
    println(
      header.names
        .map(f => normalize(f.value))
        .mkString(", ")
    )
  }

  def normalize(s: String): String = {
    if (s.isEmpty) {
      "<empty>"
    } else {
      "\"" + s + "\""
    }
  }
}
