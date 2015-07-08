import java.io._
import java.net.{InetSocketAddress, Socket, SocketTimeoutException}
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger

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

    val ips = if(args.length == 0) {
      print("Please enter the IP: ")
      Seq(StdIn.readLine())
    } else {
     args.toSeq
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

      var count = new AtomicInteger(ports.size)

      ports.map(getSocketForPort).foreach(_ onComplete {
        case Success(socket) => promise.trySuccess(socket)
        case Failure(ex) => if(count.decrementAndGet() == 0) promise.failure(ex)
      })

      promise.future
    }

    val sockets = ips.map(tryIp).map(_.map { socket =>
      goForIt(Analyzer(socket))
      socket.close()
      socket.getInetAddress
    })

    val tries = Await.result(Future.sequence(sockets.map(future2try)), Duration.Inf)
    tries.filter(_ isSuccess).foreach(addr =>  println(s"${{addr.get.toString}} worked"))
    tries.filter(_ isFailure).foreach(addr =>  println(s"${{addr.failed.get.getMessage}} didn't work"))

    System.exit(0)
  }

  def goForIt(analyzer: Analyzer) = {

    analyzer.sendCommand("v config")
    analyzer.sendCommand("t list")
    analyzer.sendCommand("v list!")

    val responseLines = analyzer.readResponses()

    val (config, rest) = responseLines.span(_.startsWith("V"))
    val (tlist, vlist) = rest.span(_.startsWith("T"))
    val responses = Seq(config.toList, tlist.toList, vlist.toList)

    // [ ]+ matches any number of spaces
    val pairsList = formatToPairs(responses)

    def isDigit(char: Char): Boolean = {
      char >= '0' && char <= '9'
    }

    val model = pairsList(0).find(pair => pair._1 == "CONFIG[0]").map(_._2).get
    val modelNr = model.dropWhile(char => char < '0' || char > '9').takeWhile(isDigit)
    val serial = pairsList(2).find(pair => pair._1 == "SERIAL_NUMBER").map(_._2).get.replace(s"$modelNr", "").filter(isDigit).dropWhile(_ == '0')
    val date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(Calendar.getInstance().getTime)

    val dir = new File("./" + modelNr)
    if(!dir.exists())
      dir.mkdir()

    val file = new File(dir, s"$serial - $model.txt")
    if(!file.exists())
      file.createNewFile()

    val out = new PrintStream(new FileOutputStream(file, true))

    out.println("Date: " + date)
    out.println(s"Model: $model, Serial: $serial")
    out.println()
    out.println(prettyFormat(pairsList))
    out.println()

    out.close()
  }

  def formatToPairs(responses: Seq[Seq[String]]): Seq[Seq[(String, String)]] = {
    responses.map(response => response.map(_.split("[ ]+").drop(3).mkString(" ")).map(param => {
      val split = param.split("=")
      split(0) -> split(1)
    }))
  }

  def prettyFormat(pairsList: Seq[Seq[(String, String)]]): String = {
    val builder = new StringBuilder()
    pairsList.foreach(pairs => {
      
      val lengthInTabs = pairs.maxBy(pair => pair._1.length)._1.length / tabInSpaces + 1
      
      pairs.foreach(pair => {
        val tabs = lengthInTabs - pair._1.length / tabInSpaces
        builder.append(pair._1).append("\t" * tabs).append(pair._2).append(System.lineSeparator())
      })
      builder.append(System.lineSeparator())
    })
    builder.mkString
  }

  def future2try[T](future: Future[T]): Future[Try[T]] = {
    future.map(Success(_)).recover{ case t: Throwable => Failure(t) }
  }
}


case class Analyzer(socket: Socket) {

  socket.setSoTimeout(Main.networkingTimeout)

  val out = new PrintStream(socket.getOutputStream)
  lazy val in = new BufferedSource(socket.getInputStream)

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

}
