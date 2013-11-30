import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.sys.process._

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Paths}
import java.util.Properties

val Native2AsciiCommand = "native2ascii -encoding %s %s"
val PropertiesFileEncoding = "UTF-8"
val PropertiesFileSuffix = ".utf8"

val path1 = args(0)
val path2 = args(1)

val propertiesFile1 = autoNative2Ascii(path1)
val propertiesFile2 = autoNative2Ascii(path2)

compareProperties(propertiesFile1, propertiesFile2)

@tailrec
def compareProperties(properties1: TreeMap[String, String],
                      properties2: TreeMap[String, String]): Unit = {
  (properties1.nonEmpty, properties2.nonEmpty) match {
    case (true, true) =>
      val key1 = properties1.firstKey
      val key2 = properties2.firstKey

      if (key1 > key2) {
        printDifference(property2 = Property(Option(key2), properties2.get(key2)))
        compareProperties(properties1, properties2 - key2)
      } else if (key1 < key2) {
        printDifference(property1 = Property(Option(key1), properties1.get(key1)))
        compareProperties(properties1 - key1, properties2)
      } else {
        (properties1.get(key1), properties2.get(key2)) match {
          case (Some(value1), Some(value2)) if value1 == value2 =>
          case (v1 @ Some(value1), v2 @ Some(value2)) =>
            printDifference(Property(Option(key1), v1), Property(Option(key2), v2))
          case other =>
        }
        compareProperties(properties1 - key1, properties2 - key2)        
      }
    case (false, _) =>
      properties2 foreach { case (k, v) => printDifference(property2 = Property(Option(k), Option(v))) }
    case (_, false) =>
      properties1 foreach { case (k, v) => printDifference(property1 = Property(Option(k), Option(v))) }
  }
}

def printDifference(property1: Property = Property(None, None),
                    property2: Property = Property(None, None)): Unit = {
  property1.key.foreach { key =>
    println("< %s = %s".format(key, property1.printableValue))
  }

  property2.key.foreach { key =>
    println("> %s = %s".format(key, property2.printableValue))
  }

  println("---")
}

def requireNative2Ascii(fileName: String): Boolean =
  PropertiesFileSuffix != null && fileName.endsWith(PropertiesFileSuffix)

def autoNative2Ascii(fileName: String): TreeMap[String, String] =
  if (requireNative2Ascii(fileName)) {
    val result = Native2AsciiCommand.format(PropertiesFileEncoding, fileName) !!

    val properties = new Properties
    properties.load(new ByteArrayInputStream(result.getBytes))
    new TreeMap[String, String] ++ properties.asScala
  } else {
    val properties = new Properties
    properties.load(new ByteArrayInputStream(Files.readAllBytes(Paths.get(fileName))))
    new TreeMap[String, String] ++ properties.asScala
  }

case class Property(key: Option[String], value: Option[String]) {
  def printableValue: String =
    value.getOrElse("") match {
      case null => ""
      case v => v.map {
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case other => other
      }.mkString
    }
}
