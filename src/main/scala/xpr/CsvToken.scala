package xpr

sealed trait Field {
  def value: String
}

final case class NonEscaped(value: String) extends Field

final case class Escaped(value: String) extends Field

case class Record(fields: IndexedSeq[Field])

case class Header(names: IndexedSeq[Field])

sealed trait File {
  def records: IndexedSeq[Record]
}

final case class HeadlessFile(records: IndexedSeq[Record]) extends File

final case class HeadedFile(records: IndexedSeq[Record], header: Header) extends File
