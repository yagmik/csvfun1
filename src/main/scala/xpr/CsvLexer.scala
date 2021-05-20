package xpr

import java.io.Reader
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.{Try, Using}

trait CsvLexer extends RegexParsers {
  override def skipWhitespace: Boolean = false

  val TEXTDATA: Regex = "[\\x20-\\x21|\\x23-\\x2B|\\x2D-\\x7E]".r
  val COMMA: String = ","
  val CR: String = "\r"
  val LF: String = "\n"
  val CRLF: String = CR + LF
  val DQUOTE = "\""
  val DDQUOTE = DQUOTE * 2

  // по стандарту non-escaped поле может быть пустым, однако в этом случае если в конце файла есть CR+LF,
  // то это расценивается как начало пустого поля. в итоге добавляется запись с одним пустым полем в конце
  // как обойти:
  // 1. СЛЕДУЯ СТАНДАРТУ:
  // а) либо фильтровать эту пустую запись (тогда завершающий opt(CRLF)) в headedFile и headlessFile не имеет смысла и избыточен)
  // б) либо удалять завершающие файл "line end"-ы - Я СДЕЛАЛ ТАК. Хотя, если в конце файла будет, например, CRLF+CRLF,
  // либо другие комбинации CR, LF, FF - это уже не соответствует стандарту.
  // 2. НЕ СЛЕДУЯ СТАНДАРТУ:
  // добавить условие в nonEscaped  - rep1 вместо rep (к проекту добавлена ANTLR4 грамматика и там так и сделано)
  val nonEscaped: Parser[NonEscaped] = rep(TEXTDATA) ^^ { xs =>
    NonEscaped(xs.mkString)
  }

  // TODO на выходе должны быть "сырые" данные, конечно
  val escaped: Parser[Escaped] = (DQUOTE ~> rep(TEXTDATA | COMMA | CR | LF | DDQUOTE) <~ DQUOTE) ^^ { xs =>
    Escaped(xs.map {
      case CR => "\\r"
      case LF => "\\n"
      case s@_ => s
    }.mkString)
  }

  val field: Parser[Field] = escaped | nonEscaped

  val name: Parser[Field] = field

  val record: Parser[Record] = (field ~ rep(COMMA ~> field)) ^^ {
    case x ~ xs => Record((x :: xs).toVector)
  }

  val header: Parser[Header] = (name ~ rep(COMMA ~> name)) ^^ {
    case x ~ xs => Header((x :: xs).toVector)
  }

  val headLessFile: Parser[HeadlessFile] = ((record ~ rep(CRLF ~> record)) <~ opt(CRLF)) ^^ {
    case x ~ xs => HeadlessFile((x :: xs).toVector)
  }

  val headedFile: Parser[HeadedFile] = ((header <~ CRLF) ~ ((record ~ rep(CRLF ~> record)) <~ opt(CRLF))) ^^ {
    case h ~ (x ~ xs) => HeadedFile((x :: xs).toVector, h)
  }

  def process(headed: Boolean, reader: Reader): Try[ParseResult[File]] = {
    val parser = if (headed) headedFile else headLessFile
    Using(reader)(parseAll(parser, _))
  }

  def process(headed: Boolean, string: String): Try[ParseResult[File]] = {
    val parser = if (headed) headedFile else headLessFile
    Try(parseAll(parser, string.stripLineEnd))
  }
}

object CsvLexer extends CsvLexer
