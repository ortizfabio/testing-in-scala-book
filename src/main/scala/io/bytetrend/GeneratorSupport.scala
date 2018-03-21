package io.bytetrend

import play.api.libs.json.{ Format, Json }
import java.io.{ByteArrayInputStream, InputStream, PrintWriter}
import java.nio.charset.StandardCharsets

import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

object GeneratorSupport {
  val logger: Logger = LoggerFactory.getLogger(GeneratorSupport.getClass)
  /**
    *
    *
    * @param fileName output file name
    * @param maxNumber How many JSON items per file.
    * @param xmlGenerator The method that will generate the POLAR XML data
    * @param read The method that will convert XML to JSON format for type T
    * @param jsonFormat a Play Formatter to translate T to JSON
    * @tparam T The type of data to be generated PhysicalVoyage, GuestBooking, Accomodation
    */
  def generateJsonFile[T](fileName: String, maxNumber: Int, xmlGenerator: (Int, Boolean, Int, Int) => String, read: InputStream => Try[Vector[T]], jsonFormat: Format[T]): Unit = {
    val xmlString: String = xmlGenerator(maxNumber, true, 0, 1)
    val data: Try[Vector[T]] = read(new ByteArrayInputStream(xmlString.getBytes(StandardCharsets.UTF_8)))
    if (data.isSuccess) {
      val vector: Vector[T] = data.getOrElse("could not generate file " + fileName).asInstanceOf[Vector[T]]
      implicit val formatter = jsonFormat
      val out: String = vector.map((x: T) => Json.prettyPrint(Json.toJson(x))).mkString("\n")
      Some(new PrintWriter(fileName)).foreach { p => p.write(out); p.close }
    } else {
      logger.warn(s"Error creating file ${fileName}")
    }
  }
}
