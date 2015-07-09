import java.io._
import java.net.{InetSocketAddress, Socket, SocketTimeoutException}
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger

import scala.StringBuilder
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
    
    val isReport = args.length != 0 && args(0) == "-p"

    val ips = if(isReport || args.length == 0) {
      print("Please enter the IP: ")
      Seq(StdIn.readLine())
    } else {
     args.toSeq
    }

    val sockets = ips.map(tryIp).map(_.map { socket =>
      val analyzer = Analyzer(socket)
      analyzer.goForIt()

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

    pairs.foreach(pair => {
      val tabs = lengthInTabs - pair._1.length / tabInSpaces
      builder.append(pair._1).append("\t" * tabs).append(pair._2).append(System.lineSeparator())
    })

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

  def goForIt() = {
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

    model = config.get.find(pair => pair._1 == "CONFIG[0]").map(_._2).get
    modelNr = model.dropWhile(!_.isDigit).takeWhile(_.isDigit)
    serial = vlist.get.find(pair => pair._1 == "SERIAL_NUMBER").map(_._2).get.replace(model.split(" ")(0), "").replaceAll("\"|\\s|-", "").dropWhile(_ == '0')
    date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(Calendar.getInstance().getTime)
  }

  def formatToPair(pair: String): (String, String) = {
    // [ ]+ matches any number of spaces
    val parts = pair.split("[ ]+").drop(3).mkString(" ").split("=")
    parts(0) -> parts(1)
  }
}
