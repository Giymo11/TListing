import java.io.{InputStream, BufferedInputStream, PrintStream}
import java.net.{InetSocketAddress, InetAddress, Socket}

import scala.concurrent.{Promise, Await, Future}
import scala.io.{BufferedSource, StdIn}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by Giymo11 on 2015-07-07 at 14:08.
 */
object Main {

  val ports = Seq(502, 3000)



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
      socket.connect(new InetSocketAddress(ip, port), 5000)
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
    analyzer.response.getLines().map(_.split("[ ]+").drop(3).mkString(" ")).map(param => {
      val split = param.split("=")
      split(0) -> split(1)
    }).foreach(println)

    println("Response printed")
  }
}


case class Analyzer(socket: Socket) {

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
