import scala.collection.mutable.ListBuffer

import java.nio.file.Path

trait CompareResult {
  def message: String

  protected def escapeValue(value: String): String =
    value match {
      case null => ""
      case other =>
        other.map {
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '\t' => "\\t"
          case other => other
        }.mkString
    }
}

class DifferenceBuffer(leftPath: Path, rightPath: Path) {
  private[this] val differencePartBuffer: ListBuffer[DifferencePart] = new ListBuffer

  def +=(difference: DifferencePart): this.type = {
    differencePartBuffer += difference
    this
  }

  def toDifferenceOrEquals: CompareResult = differencePartBuffer toList match {
    case Nil => Equals(leftPath, rightPath)
    case differences => Difference(leftPath, rightPath, differences)
  }
}

case class Equals(leftPath: Path, rightPath: Path) extends CompareResult {
  def message: String = ""
}

case class Difference(leftPath: Path, rightPath: Path, differences: List[DifferencePart]) extends CompareResult {
  def message: String =
    differences
      .map(_.message)
      .mkString(System.lineSeparator + "---" + System.lineSeparator)
}

case class DifferencePart(leftKey: Option[String], leftValue: Option[String],
                          rightKey: Option[String], rightValue: Option[String]) extends CompareResult {
  def message: String =
    List((leftKey, leftValue), (rightKey, rightValue)).zip(List("< %s = %s", "> %s = %s"))
      .filter { case (c, _) => c._1.isDefined }
      .map { case (c, f) => f.format(c._1.get, escapeValue(c._2.getOrElse(""))) }
      .mkString(System.lineSeparator)
}

case class OnlyFile(path: Path) extends CompareResult {
  def message: String =
    "Onln in %s: %s".format(path.getParent, path.getFileName)
}
