import scala.annotation.tailrec
import scala.collection.SortedMap

import java.nio.file.{Files, Path, Paths}

object PropertiesFileComparator {
  def main(args: Array[String]): Unit = {
    val (leftTarget, rightTarget) = args toList match {
      case left :: right :: Nil => (left, right)
      case other => usage()
    }

    val leftPath = Paths get leftTarget match {
      case left if Files.exists(left) => left
      case left => noSuchFileOrDirectoryExit(left)
    }

    val rightPath = Paths get rightTarget match {
      case right if Files.exists(right) => right
      case right => noSuchFileOrDirectoryExit(right)
    }

    val propertiesFileComparator =
      (Files.isDirectory(leftPath) && Files.isDirectory(rightPath)) ||
      (!Files.isDirectory(leftPath) && !Files.isDirectory(leftPath)) match {
        case true => new PropertiesFileComparator(leftPath, rightPath)
        case false => fileTypeNotMatchExit(leftPath, rightPath)
      }

    propertiesFileComparator.compare.foreach {
      case Equals(_, _) =>
      case d @ Difference(leftPath, rightPath, _) =>
        printf("Found Difference %s <-> %s%n", leftPath, rightPath)
        println(d.message)
      case of @ OnlyFile(_) => println(of.message)
    }
  }

  private def usage(): Nothing = {
    val message = """|This Tool Required 2 Arguments
                     |  1: Compare Properties File or Directory
                     |  2: Compare Propertirs File or Directory""".stripMargin
    println(message)
    sys.exit(0)
  }

  private def noSuchFileOrDirectoryExit(path: Path): Nothing = {
    printf("No Such File or Directory [%s]%n", path)
    sys.exit(1)
  }

  private def fileTypeNotMatchExit(left: Path, right: Path): Nothing = {
    val leftMessage =
                  if (Files.isDirectory(left)) "Directory"
                  else "File"
    val rightMessage =
                  if (Files.isDirectory(right)) "Directory"
                  else "File"

    printf("%s is %s, But %s is %s%n", left, leftMessage, right, rightMessage)
    sys.exit(1)
  }
}

class PropertiesFileComparator(leftPath: Path, rightPath: Path) {
  def compare(): List[CompareResult] = {
    val propertiesFileFinder = new PropertiesFileFinder
    (propertiesFileFinder.find(leftPath), propertiesFileFinder.find(rightPath)) match {
      case (leftPropertiesFile :: Nil, rightPropertiesFile :: Nil) =>
        List(compareProperties(leftPropertiesFile, rightPropertiesFile))
      case (leftPropertiesFiles, rightPropertiesFiles) =>
        compareFiles(leftPropertiesFiles, rightPropertiesFiles, Nil)
    }
  }

  @tailrec
  private def compareFiles(leftFiles: List[PropertiesFile],
                           rightFiles: List[PropertiesFile],
                           results: List[CompareResult]): List[CompareResult] = {
    (leftFiles, rightFiles) match {
      case (leftFile :: leftRest, rightFile :: rightRest)
        if leftFile.path.toFile.getName > rightFile.path.toFile.getName =>
        compareFiles(leftFiles, rightRest, OnlyFile(rightFile.path) :: results)
      case (leftFile :: leftRest, rightFile :: rightRest)
        if leftFile.path.toFile.getName < rightFile.path.toFile.getName =>
        compareFiles(leftRest, rightFiles, OnlyFile(leftFile.path) :: results)
      case (leftFile :: leftRest, rightFile :: rightRest) =>
        compareFiles(leftRest, rightRest, compareProperties(leftFile, rightFile) :: results)
      case (Nil, Nil) => results reverse
      case (leftFile :: leftRest, Nil) => compareFiles(Nil, Nil, OnlyFile(leftFile.path) :: results)
      case (Nil, rightFile :: rightRest) => compareFiles(Nil, Nil, OnlyFile(rightFile.path) :: results)
    }
  }

  private def compareProperties(leftFile: PropertiesFile, rightFile: PropertiesFile): CompareResult = {
    val buffer = new DifferenceBuffer(leftFile.path, rightFile.path)

    @tailrec
    def comparePropertiesInner(leftProperties: SortedMap[String, String],
                             rightProperties: SortedMap[String, String]): CompareResult = {
      if (leftProperties.nonEmpty && rightProperties.nonEmpty) {
        val leftKey = leftProperties.firstKey
        val rightKey = rightProperties.firstKey
        val lkOption = Some(leftKey)
        val rkOption = Some(rightKey)

        if (leftKey > rightKey) {
          buffer += DifferencePart(None, None, rkOption, rightProperties.get(rightKey))
          comparePropertiesInner(leftProperties, rightProperties - rightKey)
        } else if (leftKey < rightKey) {
          buffer += DifferencePart(lkOption, leftProperties.get(leftKey), None, None)
          comparePropertiesInner(leftProperties - leftKey, rightProperties)
        } else { // (leftKey == rightKey) {
          (leftProperties.get(leftKey), rightProperties.get(rightKey)) match {
            case (Some(lv), Some(rv)) if lv == rv =>
            case (l @ Some(lv), r @ Some(rv)) if lv != rv =>
              buffer += DifferencePart(lkOption, l, rkOption, r)
            case other =>
            /*
            case (l @ None, r @ Some(_)) =>
              buffer += DifferencePart(lkOption, l, rkOption, r)
            case (l @ Some(_), r @ None) =>
              buffer += DifferencePart(lkOption, l, rkOption, r)
            case (None, None) =>
            */
          }
          comparePropertiesInner(leftProperties - leftKey, rightProperties - rightKey)
        }
      } else if (leftProperties.isEmpty) {
        if (rightProperties.nonEmpty) {
          for ((k, v) <- rightProperties)
            buffer += DifferencePart(None, None, Some(k), Option(v))
        }

        buffer toDifferenceOrEquals
      } else if (rightProperties.isEmpty) {
        if (leftProperties.nonEmpty) {
          for ((k, v) <- leftProperties)
            buffer += DifferencePart(Some(k), Option(v), None, None)
        }

        buffer toDifferenceOrEquals
      } else {
        buffer toDifferenceOrEquals
      }
    }

    comparePropertiesInner(leftFile.properties, rightFile.properties)
  }
}

case class PropertiesFile(path: Path, properties: SortedMap[String, String])
