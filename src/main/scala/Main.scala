import java.io._
import java.net.{InetSocketAddress, Socket, SocketTimeoutException}
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.io.{BufferedSource, StdIn}

/**
 * Created by Giymo11 on 2015-07-07 at 14:08.
 */
object Main {

  val ports = Seq(502, 3000)
  val networkingTimeout = 5000
  val tabInSpaces = 8

  def main(args: Array[String]): Unit = {

    val ip = if(args.length == 0) {
      print("Please enter the IP: ")
      StdIn.readLine()
    } else {
     args(0)
    }

    println()

    def getSocketForPort(port: Int): Future[Socket] = Future {
      val socket = new Socket()
      socket.connect(new InetSocketAddress(ip, port), networkingTimeout)
      socket
    }

    val promise = Promise[Socket]()

    ports.foreach( getSocketForPort(_) onSuccess {
      case socket => try {
        promise.success(socket)
      } catch {
        case ex: Exception => //left blank intentionally
      }
    })

    try {
      import scala.concurrent.duration._
      val analyzer = Analyzer(Await.result(promise.future, 5 seconds))
      goForIt(analyzer)
      analyzer.socket.close()
    } catch {
      case ex: Exception =>
        println("Could not reach " + ip + " at ports " + ports.mkString(", "))
    }
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

    val model = pairsList(0).find(pair => pair._1 == "CONFIG[0]").map(_._2).get
    val serial = pairsList(2).find(pair => pair._1 == "SERIAL_NUMBER").map(_._2).get.replace("\"", "").dropWhile(_ == "0")

    println("Date: " + new SimpleDateFormat("yyyy-MM-dd").format(Calendar.getInstance().getTime))
    println(s"Model: $model, Serial: $serial \n")

    println(prettyFormat(pairsList))
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
        builder.append(pair._1).append("\t" * tabs).append(pair._2).append("\n")
      })
      
      builder.append("\n")
    })
    builder.mkString
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
      while (in.hasNext)
        builder.append(in.next())
    } catch {
      case ex: SocketTimeoutException => //left blank intentionally
    }
    new BufferedSource(new ByteArrayInputStream(builder.mkString.getBytes)).getLines()
  }

}
