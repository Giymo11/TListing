import java.io._
import java.net.{SocketTimeoutException, InetSocketAddress, InetAddress, Socket}
import java.nio.CharBuffer
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.concurrent.{Promise, Await, Future}
import scala.io.{BufferedSource, StdIn}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by Giymo11 on 2015-07-07 at 14:08.
 */
object Main {

  val ports = Seq(502, 3000)
  val networkingTimeout = 5000

  def main(args: Array[String]): Unit = {

    val ip = if(args.length == 0) {
      print("Please enter the IP: ")
      StdIn.readLine()
    } else {
     args(0)
    }

    println("Hostname: " + ip)

    import scala.concurrent.duration._

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
      val analyzer = Analyzer(Await.result(promise.future, 5 seconds))

      goForIt(analyzer)

      analyzer.socket.close()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        println("Could not reach " + ip + " at ports " + ports.mkString(", ") + " because of " + ex.getMessage)
    }
  }

  def goForIt(analyzer: Analyzer) = {
    analyzer.sendCommand("v config")
    analyzer.sendCommand("t list")
    analyzer.sendCommand("v list!")

    // [ ]+ matches any number of spaces

    val builder = new StringBuilder()
    try {
      while (analyzer.response.hasNext)
        builder.append(analyzer.response.next())
    } catch {
      case ex: SocketTimeoutException => println("Read timed out (as expected)")//do nothing
    }

    val response = new BufferedSource(new ByteArrayInputStream(builder.mkString.getBytes))

    println("String converted")

    val pairs = response.getLines().map(_.split("[ ]+").drop(3).mkString(" ")).map(param => {
      val split = param.split("=")
      split(0) -> split(1)
    })

    println("Response received")

    val map = pairs.toMap

    println("Map created")

    val model = map("CONFIG[0]")
    val serial = map("SERIAL_NUMBER").replace("\"", "")

    println("Date: " + new SimpleDateFormat("yyyy-mm-dd").format(Calendar.getInstance().getTime))
    println(s"Model $model, Serial: $serial \n\n")

    map.foreach(pair => println(s"${{pair._1}}\t${{pair._2}}"))

    println("Response printed")
  }
}


case class Analyzer(socket: Socket) {

  socket.setSoTimeout(Main.networkingTimeout)

  val out = new PrintStream(socket.getOutputStream)
  lazy val in = new BufferedSource(socket.getInputStream)

  def sendCommand(command: String): Unit = {
    println("Sending: " + command)
    out.println(command)
    out.flush()
    println("Sent")
  }

  def response = in
}
