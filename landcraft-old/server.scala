package server

import java.net._
import java.io._
import scala.util._
import util.Serial._

private class Session(sock: Socket) {
  var name: String = ""
  val out = new DataOutputStream(clnt.getOutputStream)
  val in = new DataInputStream(clnt.getInputStream)
  def close() {
    out.close()
  }
  def readMessage(): Message = implicitly[Serial[Message]].read(in)
  def writeMessage(msg: Message) {
    implicitly[Serial[Message]].write(out, msg)
  }
  def expect[A <: Message](): A = {
    while (true) {
      val m = implicitly[Serial[Message]].read(in)
      try { return m.asInstanceOf[A] }
      catch { case _: ClassCastException => }
    }
  }
}

class LcServer(port: Int) {
  private val sock = new ServerSocket(port)
  private var turn = -1
  private val sess = collection.mutable.ArrayBuffer[Session]()
  private val game = new Game(new CoordSys(10))
  def run() {
    Future { listen() }
    val s = this.synchronized {
      turn += 1
      sess(turn)
    }
    s.writeMessage(YourTurn)
    val GameCommand(cmd) = s.expect[GameCommand]()

  def listen() {
    while (true) {
      val clnt = new Session(sock.accept())
      Future {
        val Join(name) = clnt.expect[Join]()
        this.synchronized {
          for (c <- sess) c.writeMessage(Join(name))
          clnt.name = name
          val cmd = Spawn(game.genSpawnLocation())
          game.runCommand(cmd)
          sess += clnt
          for (c <- sess) c.writeMessage(GameCommand(cmd))
        }
      }
    }
  }
