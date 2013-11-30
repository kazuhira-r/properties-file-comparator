import scala.collection.SortedMap
import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer
import scala.sys.process._

import java.io.ByteArrayInputStream
import java.nio.file.{Files, FileVisitResult, Path, Paths, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes
import java.util.Properties

object PropertiesFileFinder {
  val Native2AsciiCommand = "native2ascii -encoding %s %s"
}

class PropertiesFileFinder(encoding: Option[String] = Some("UTF-8"),
                           suffix: Option[String] = Some(".utf8"),
                           autoNative2Ascii: Boolean = true) {
  def find(path: Path): List[PropertiesFile] = {
    val visitor = new PropertiesFileVisitor(path, toProperties)
    Files.walkFileTree(path, visitor)
    visitor.propertiesFiles
  }

  private def toProperties(file: Path): PropertiesFile = {
    val applyNative2Ascii = suffix exists { s =>
      file.toString.endsWith(s) && autoNative2Ascii
    }

    val props = new Properties
    val bytes = applyNative2Ascii match {
      case true =>
        val enc = encoding.getOrElse(throw new IllegalArgumentException("Not Set Encoding!"))
        val result =
                PropertiesFileFinder.Native2AsciiCommand.format(enc, file) !!  // execute command

        result.getBytes(enc)
      case false => Files.readAllBytes(file)
    }

    props.load(new ByteArrayInputStream(bytes))

    if (applyNative2Ascii) {
      val fileName = file.toFile.getName
      val parent = Option(file.getParent)
      val newFile = Paths.get(fileName.substring(0, fileName.size - suffix.get.size))
      PropertiesFile(
        parent.getOrElse(Paths.get("")).resolve(newFile),
        new TreeMap[String, String] ++ props.asScala
      )
    } else {
      PropertiesFile(file, new TreeMap[String, String] ++ props.asScala)
    }
  }
}

private class PropertiesFileVisitor(path: Path, toProperties: (Path => PropertiesFile))
        extends SimpleFileVisitor[Path] {
  private[this] val propertiesFilesBuffer: ListBuffer[PropertiesFile] = new ListBuffer

  override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
    file toFile match {
      case f =>
      // case f if f.getName.indexOf(".properties") != -1 =>
        val propertiesFile = toProperties(file)
        propertiesFilesBuffer += PropertiesFile(propertiesFile.path, propertiesFile.properties)
        FileVisitResult.CONTINUE
      // case other => FileVisitResult.CONTINUE
    }

  def propertiesFiles: List[PropertiesFile] = propertiesFilesBuffer toList
}
