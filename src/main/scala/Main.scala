import java.io._
import java.net.{InetSocketAddress, Socket, SocketTimeoutException}
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger

import org.apache.poi.xssf.usermodel.{XSSFCell, XSSFSheet, XSSFWorkbook}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.io.{BufferedSource, StdIn}
import scala.util.{Failure, Success, Try}

case class Analyzer(socket: Socket) {

  socket.setSoTimeout(Main.networkingTimeout)
  // important (156)

  lazy val in = new BufferedSource(socket.getInputStream)
  val out = new PrintStream(socket.getOutputStream)
  var model, modelNr, modelFamily, serial, timestamp, date = ""
  var config, tlist, vlist: Option[Seq[(String, String)]] = None

  def initializeViaNetwork() = {
    sendCommand("v config")
    sendCommand("t list")
    sendCommand("v list!")

    val responseLines = readResponses()

    socket.close()

    val (configString, rest) = responseLines.span(_.startsWith("V")) // important
    val (tlistString, vlistString) = rest.span(_.startsWith("T"))

    def isPair(string: String) = string.contains("=")

    this.config = Some(configString.toSeq.filter(isPair).map(formatToPair))
    this.tlist = Some(tlistString.toSeq.filter(isPair).map(formatToPair))
    this.vlist = Some(vlistString.toSeq.filter(isPair).map(formatToPair))

    model = config.get.find { case (name, value) => name == "CONFIG[0]" }.map(_._2).get
    modelNr = model.takeWhile(_ != ' ')
    modelFamily = model.dropWhile(!_.isDigit).takeWhile(_.isDigit)
    serial = vlist.get.find { case (name, value) => name == "SERIAL_NUMBER" }.map(_._2).get.replace(model.split(" ")(0), "").replaceAll("\"|\\s|-", "").dropWhile(_ == '0')
    val binaryDate = Calendar.getInstance().getTime
    timestamp = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(binaryDate)
    date = new SimpleDateFormat("yyyy-MM-dd").format(binaryDate)
  }

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

  def formatToPair(pair: String): (String, String) = {
    // [ ]+ matches any number of spaces
    val parts = pair.split("[ ]+").drop(3).mkString(" ").split("=")
    parts(0) -> parts(1)
  }
}

/**
 * Created by Giymo11 on 2015-07-07 at 14:08.
 */
object Main {

  val ports = Seq(502, 3000)
  val networkingTimeout = 5000
  val tabInSpaces = 8

  val coverSheetName = "Cover"
  val modelRow = 9
  val modelColumn = 'J'
  val serialRow = 11
  val serialColumn = 'J'
  val entryDateRow = 12
  val entryDateColumn = 'D'
  val exitDateRow = 12
  val exitDateColumn = 'J'

  val paramSheetName = "Parameter"
  val paramRowStart = 3
  val paramRowEnd = 100
  val parameterColumn = 'B'
  val entryColumn = 'E'
  val exitColumn = 'G'

  val vlistSheetName = "Vlist"
  val vlistRowStart = 3
  val vlistNameColumn = 'A'
  val vlistValueColumn = 'C'

  val finishedReportsDirectory = "./finished"
  val excelEnding = "xlsm"

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
      analyzer.initializeViaNetwork()

      archive(analyzer)
      if (isReport) {
        report(analyzer, "Deutsch")
        report(analyzer, "English")
      }

      socket.getInetAddress
    })

    val tries = Await.result(Future.sequence(sockets.map(future2try)), Duration.Inf)
    tries.filter(_ isSuccess).foreach(addr =>  println(s"${{addr.get.toString}} worked"))
    tries.filter(_ isFailure).foreach(addr => {
      //addr.failed.get.printStackTrace()
      println(s"${{addr.failed.get.getMessage}} didn't work")
    }) // addr.failed.get.printStackTrace()) //

    System.exit(0)
  }

  def report(analyzer: Analyzer, postfix: String): AnyVal = {
    // check if entry or exit
    val reportName = s"Report for ${{analyzer.serial}} - ${{analyzer.model}} $postfix.$excelEnding"
    val reportFile = new File(reportName)

    val isEntry = !reportFile.exists()

    var reportInputStream: Option[InputStream] = None
    val template = if (isEntry) getEntryTemplate(analyzer, postfix) else {
      reportInputStream = Some(new FileInputStream(reportFile))
      new XSSFWorkbook(reportInputStream.get)
    }

    val parameterSheet = template.getSheet(Main.paramSheetName)
    val coverSheet = template.getSheet(Main.coverSheetName)

    if (isEntry) {
      println("Recording entry of " + analyzer.serial)
      getCellAt(coverSheet, modelRow, modelColumn).setCellValue(analyzer.model)
      getCellAt(coverSheet, serialRow, serialColumn).setCellValue(analyzer.serial)
      getCellAt(coverSheet, entryDateRow, entryDateColumn).setCellValue(analyzer.date)
      insertParameters(analyzer, parameterSheet, entryColumn)
    } else {
      println("Recording exit of " + analyzer.serial)
      getCellAt(coverSheet, exitDateRow, exitDateColumn).setCellValue(analyzer.date)
      insertVlist(analyzer, if (template.getSheet(vlistSheetName) == null) template.createSheet(vlistSheetName) else template.getSheet(vlistSheetName))
      insertParameters(analyzer, parameterSheet, exitColumn)
    }

    val out = new FileOutputStream(
      if (!isEntry)
        getFile(finishedReportsDirectory, reportName.replace(s" $postfix.$excelEnding", "") + " - finished " + analyzer.date + s" - $postfix.$excelEnding")
      else
        reportFile
    )

    template.write(out)
    out.flush()
    out.close()
    reportInputStream.foreach(_ close())

    if (!isEntry) {
      println("Moving " + reportFile.getName)
      reportFile.delete()
    }
  }

  def getEntryTemplate(analyzer: Analyzer, postfix: String): XSSFWorkbook = {
    val templateName = s"Template for ${{analyzer.modelNr}} $postfix.$excelEnding"
    val templateFile = new File(templateName)
    if (!templateFile.exists()) {
      println(s"$templateName does not exist!")
      throw new scala.Exception(analyzer.socket.getInetAddress.toString)
    }
    templateFile.setReadOnly()
    new XSSFWorkbook(templateFile)
  }

  def insertParameters(analyzer: Analyzer, reportSheet: XSSFSheet, column: Int) = {
    // get wanted parameters
    val parameterPairs = for (row <- paramRowStart to paramRowEnd) yield getCellAt(reportSheet, row, parameterColumn).getStringCellValue -> row
    val parameterMap = parameterPairs.toMap

    def insertPair(pair: (String, String), hasUnits: Boolean) = {
      val (name, value) = pair
      parameterMap.get(name) match {
        case Some(row) =>
          val cell = getCellAt(reportSheet, row, column)
          if (hasUnits) cell.setCellValue(value.takeWhile(_ != ' ').toDouble) // drop the units on the pairs
          else cell.setCellValue(value)
        case None => // left blank intentionally
      }
    }

    analyzer.config.get.foreach(insertPair(_, hasUnits = false))
    analyzer.tlist.get.foreach(insertPair(_, hasUnits = true))
  }

  def insertVlist(analyzer: Analyzer, vlistSheet: XSSFSheet): Unit = {
    val vlist = analyzer.vlist.get
    val wrapText = vlistSheet.getWorkbook.createCellStyle()
    wrapText.setWrapText(true)

    for (row <- vlist.indices) {
      val current = vlist(row)
      getCellAt(vlistSheet, row + vlistRowStart, vlistNameColumn).setCellValue(current._1)
      getCellAt(vlistSheet, row + vlistRowStart, vlistValueColumn).setCellStyle(wrapText)
      getCellAt(vlistSheet, row + vlistRowStart, vlistValueColumn).setCellValue(current._2)
    }
    vlistSheet.autoSizeColumn(vlistNameColumn - 'A')
  }

  def getCellAt(sheet: XSSFSheet, rowIndex: Int, columnIndex: Int): XSSFCell = {
    var row = sheet.getRow(rowIndex - 1)
    if (row == null) row = sheet.createRow(rowIndex - 1)
    var cell = row.getCell(columnIndex - 'A')
    if (cell == null) cell = row.createCell(columnIndex - 'A')
    cell
  }

  def getFile(dirName: String, fileName: String) = {
    val dir = new File(dirName)
    if (!dir.exists())
      dir.mkdir()

    val file = new File(dir, fileName)

    if (!file.exists())
      file.createNewFile()

    file
  }

  def archive(analyzer: Analyzer) = {
    val file = getFile("./" + analyzer.modelFamily, s"${{analyzer.serial}} - ${{analyzer.model}}.txt")

    val in = new BufferedSource(new FileInputStream(file))
    val oldContent = in.getLines().toList
    in.close()

    val builder = new StringBuilder(oldContent.size * 2)

    val linebreak = System.lineSeparator()
    builder.append("Date: " + analyzer.timestamp).append(linebreak)
    builder.append(s"Model: ${{analyzer.model}}, Serial: ${{analyzer.serial}}").append(linebreak * 2)
    builder.append(prettyFormat(analyzer.config.get)).append(linebreak)
    builder.append(prettyFormat(analyzer.tlist.get)).append(linebreak)
    builder.append(prettyFormat(analyzer.vlist.get)).append(linebreak * 3)
    builder.append(oldContent.mkString(linebreak))

    val out = new FileOutputStream(file, false) // important
    out.write(builder.mkString.getBytes)
    out.close()
  }

  def prettyFormat(pairs: Seq[(String, String)]): String = {
    val builder = new StringBuilder()

    val lengthInTabs = pairs.maxBy(pair => pair._1.length)._1.length / tabInSpaces + 1

    pairs.foreach { case (name, value) =>
      val tabs = lengthInTabs - name.length / tabInSpaces
      builder.append(name).append("\t" * tabs).append(value).append(System.lineSeparator())
    }

    builder.mkString
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
      case Failure(ex) => if (count.decrementAndGet() == 0) promise.failure(ex)
    })

    promise.future
  }

  def future2try[T](future: Future[T]): Future[Try[T]] = {
    future.map(Success(_)).recover{ case t: Throwable => Failure(t) }
  }
}
