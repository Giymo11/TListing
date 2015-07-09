import java.io._
import java.net.{InetSocketAddress, Socket, SocketTimeoutException}
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger

import org.apache.poi.openxml4j.opc.OPCPackage
import org.apache.poi.xssf.usermodel.{XSSFCell, XSSFSheet, XSSFWorkbook}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.io.{BufferedSource, StdIn}
import scala.util.{Try, Failure, Success}

/**
 * Created by Giymo11 on 2015-07-07 at 14:08.
 */
object Main {

  val ports = Seq(502, 3000)
  val networkingTimeout = 5000
  val tabInSpaces = 8

  def main(args: Array[String]): Unit = {

    import scala.concurrent.duration._
    
    val isReport = args.length != 0 && args(0) == "-r"

    val ips = if(isReport || args.length == 0) {
      print("Please enter the IP: ")
      Seq(StdIn.readLine())
    } else {
     args.toSeq
    }
    
    val sockets = ips.map(tryIp).map(_.map { socket =>
      val analyzer = Analyzer(socket)
      analyzer.getParametersAndInitialize()
      
      val file = getFile("./" + analyzer.modelNr, s"${{analyzer.serial}} - ${{analyzer.model}}.txt")
      
      val in = new BufferedSource(new FileInputStream(file))
      val oldContent = in.getLines().toList
      in.close()

      val builder = new StringBuilder(oldContent.size * 2)

      val linebreak = System.lineSeparator()
      builder.append("Date: " + analyzer.date).append(linebreak)
      builder.append(s"Model: ${{analyzer.model}}, Serial: ${{analyzer.serial}}").append(linebreak * 2)
      builder.append(prettyFormat(analyzer.config.get)).append(linebreak)
      builder.append(prettyFormat(analyzer.tlist.get)).append(linebreak)
      builder.append(prettyFormat(analyzer.vlist.get)).append(linebreak * 3)
      builder.append(oldContent.mkString(linebreak))

      val out = new FileOutputStream(file, false) // important
      out.write(builder.mkString.getBytes)
      out.close()

      if(isReport) {

        // check if entry or exit
        val reportName = s"Report for ${{analyzer.serial}} - ${{analyzer.model}}.xlsx"
        val reportFile = new File(reportName)

        if (!reportFile.exists()) { // its an entry

          println("Recording entry of " + analyzer.serial)

          val modelRow = 5
          val modelColumn = 'K'
          val serialRow = 7
          val serialColumn = 'K'

          val paramRowStart = 14
          val paramRowEnd = 42
          val parameterColumn = 'B'
          val entryColumn = 'E'
          val exitColumn = 'I'

          // search for template
          val templateName = s"Template for ${{analyzer.modelNr}}.xlsx"
          val templateFile = new File(templateName)
          if(!templateFile.exists()) {
            println(s"$templateName does not exist!")
            throw new Exception(socket.getInetAddress.toString)
          }

          val template = new XSSFWorkbook(templateFile)

          val reportSheet = template.getSheetAt(0)

          def getCellAt(sheet: XSSFSheet, row: Int, column: Int): XSSFCell = {
            sheet.getRow(row - 1).getCell(column - 'A')
          }

          getCellAt(reportSheet, modelRow, modelColumn).setCellValue(analyzer.model)
          getCellAt(reportSheet, serialRow, serialColumn).setCellValue(analyzer.serial)

          val parameterPairs = for(row <- paramRowStart to paramRowEnd) yield getCellAt(reportSheet, row, parameterColumn).getStringCellValue -> row
          val parameterMap = parameterPairs.toMap

          def insertPair(pair: (String, String)) = {
            val (name, value) = pair
            parameterMap.get(name) match {
              case Some(row) => getCellAt(reportSheet, row, entryColumn).setCellValue(value.takeWhile(_ != ' '))
              case None => // left blank intentionally
            }
          }

          analyzer.config.get.foreach(insertPair)
          analyzer.tlist.get.foreach(insertPair)

          reportFile.createNewFile()
          val out = new FileOutputStream(reportFile)
          template.write(out)
          template.close()
          out.close()

        } else { // its an exit

          println("Recording exit of " + analyzer.serial)

          val out = new FileOutputStream(reportFile)
          out.close()
        }

        // get wanted parameters
        // drop the units on the pairs
        // write down parameters (only config and tlist)
        // if exit, append vlist
        // save appropriately
      }

      socket.getInetAddress
    })

    val tries = Await.result(Future.sequence(sockets.map(future2try)), Duration.Inf)
    tries.filter(_ isSuccess).foreach(addr =>  println(s"${{addr.get.toString}} worked"))
    tries.filter(_ isFailure).foreach(addr =>  println(s"${{addr.failed.get.getMessage}} didn't work"))

    System.exit(0)
  }

  def tryIp(ip: String): Future[Socket] = {
    def getSocketForPort(port: Int): Future[Socket] = Future {
      val socket = new Socket()
      val addr = new InetSocketAddress(ip, port)
      try {
        socket.connect(addr, networkingTimeout)
      } catch {
        case ex: Exception => throw new Exception(addr.getAddress.toString)
      }
      socket
    }

    val promise = Promise[Socket]()

    val count = new AtomicInteger(ports.size)

    ports.map(getSocketForPort).foreach(_ onComplete {
      case Success(socket) => promise.trySuccess(socket)
      case Failure(ex) => if(count.decrementAndGet() == 0) promise.failure(ex)
    })

    promise.future
  }

  def getFile(dirName: String, fileName: String) = {
    val dir = new File(dirName)
    if(!dir.exists())
      dir.mkdir()

    val file = new File(dir, fileName)

    if(!file.exists())
      file.createNewFile()

    file
  }

  def prettyFormat(pairs: Seq[(String, String)]): String = {
    val builder = new StringBuilder()

    val lengthInTabs = pairs.maxBy(pair => pair._1.length)._1.length / tabInSpaces + 1

    pairs.foreach { case (name, value) => {
      val tabs = lengthInTabs - name.length / tabInSpaces
      builder.append(name).append("\t" * tabs).append(value).append(System.lineSeparator())
    }}

    builder.mkString
  }

  def future2try[T](future: Future[T]): Future[Try[T]] = {
    future.map(Success(_)).recover{ case t: Throwable => Failure(t) }
  }
}

case class Analyzer(socket: Socket) {

  socket.setSoTimeout(Main.networkingTimeout) // important (156)

  val out = new PrintStream(socket.getOutputStream)
  lazy val in = new BufferedSource(socket.getInputStream)

  var model, modelNr, serial, date = ""
  var config, tlist, vlist : Option[Seq[(String, String)]] = None

  def sendCommand(command: String): Unit = {
    out.println(command)
    out.flush()
  }

  def readResponses() = {
    val builder = new StringBuilder()
    try {
      while (true)
        builder.append(in.next())
    } catch {
      case ex: SocketTimeoutException => //left blank intentionally
    }
    new BufferedSource(new ByteArrayInputStream(builder.mkString.getBytes)).getLines()
  }

  def getParametersAndInitialize() = {
    sendCommand("v config")
    sendCommand("t list")
    sendCommand("v list!")

    val responseLines = readResponses()

    socket.close()

    val (configString, rest) = responseLines.span(_.startsWith("V")) // important
    val (tlistString, vlistString) = rest.span(_.startsWith("T"))

    this.config = Some(configString.toSeq.map(formatToPair))
    this.tlist = Some(tlistString.toSeq.map(formatToPair))
    this.vlist = Some(vlistString.toSeq.map(formatToPair))

    model = config.get.find { case (name, value) => name == "CONFIG[0]" }.map(_._2).get
    modelNr = model.dropWhile(!_.isDigit).takeWhile(_.isDigit)
    serial = vlist.get.find { case (name, value) => name == "SERIAL_NUMBER" }.map(_._2).get.replace(model.split(" ")(0), "").replaceAll("\"|\\s|-", "").dropWhile(_ == '0')
    date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(Calendar.getInstance().getTime)
  }

  def formatToPair(pair: String): (String, String) = {
    // [ ]+ matches any number of spaces
    val parts = pair.split("[ ]+").drop(3).mkString(" ").split("=")
    parts(0) -> parts(1)
  }
}
