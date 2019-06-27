package model

import scala.collection.mutable._
import util._

package object Model {
  type Coord = (Int, Int)
  type Uid = Int
}

import Model._

sealed trait Dir
case object Up extends Dir
case object Down extends Dir
case object Left extends Dir
case object Right extends Dir

class CoordSys(val size: Int) {
  def check(pos: Coord): Boolean =
    pos._1 >= 0 && pos._1 < size &&
    pos._2 >= 0 && pos._2 < size
  def apply(pos: Coord) = check(pos)
  def move(pos: Coord, dir: Dir): Coord = {
    val newpos = dir match {
      case Up => (pos._1, pos._2 + 1)
      case Down => (pos._1, pos._2 - 1)
      case Left => (pos._1 - 1, pos._2)
      case Right => (pos._1 + 1, pos._2)
    }
    if (check(newpos)) newpos
    else pos
  }
  def distance(a: Coord, b: Coord): Int = {
    val sq = (x: Int) => x * x
    Math.sqrt(sq(a._1 - b._1) + sq(a._2 - b._2)).ceil.toInt
  }
}

sealed trait Command[+A]
case class Spawn(pos: Coord) extends Command[Uid]
case class Move(uid: Uid, dir: Dir) extends Command[Unit]
case class Suicide(uid: Uid) extends Command[Unit]

sealed trait Program[+A] {
  def map[B](f: A => B): Program[B] =
    this match {
      case Pure(a) => Pure(f(a))
      case Bind(m, cont) => Bind(m, cont map (_ map f))
    }
  def flatMap[B](f: A => Program[B]): Program[B] =
    this match {
      case Pure(a) => f(a)
      case Bind(m, cont) => Bind(m, cont map (_ flatMap f))
    }
  def then[B](m: Program[B]): Program[B] =
    flatMap(_ => m)
}
case class Pure[+A](value: A) extends Program[A]
case class Bind[A, +B](value: Command[A], cont: A => Program[B]) extends Program[B]

object Program {
  def pure[A](a: A): Program[A] = Pure(a)
  def spawn(pos: Coord): Program[Uid] = Bind(Spawn(pos), pure[Uid])
  def move(uid: Uid, dir: Dir): Program[Unit] = Bind(Move(uid, dir), pure[Unit])
  def suicide(uid: Uid): Program[Unit] = Bind(Suicide(uid), pure[Unit])
  def lift[A](cmd: Command[A]): Program[A] = Bind(cmd, pure[A])
  def isSuicide(prog: Program[Any]): Boolean =
    prog match {
      case Bind(Suicide(_), _) => true
      case _ => false
    }
}

class SimulationError(msg: String) extends RuntimeException(msg)

class Game(val coordsys: CoordSys) {
  val units = Map[Uid, LcUnit]()
  var myUid: Option[Uid] = None
  val commands = new EventBus[Command[Any]]
  def runProgram[A](prog: Program[A]): A =
    prog match {
      case Pure(a) => a
      case Bind(m, f) => run(f(runCommand(m)))
    }
  def runCommand[A](cmd: Command[A]): A = {
    commands.fire(cmd)
    cmd match {
      case Spawn(pos) =>
        if (findByPos(pos).isDefined)
          throw new SimulationError("invalid spawning position")
        val uid = units.size
        units += uid -> new LcUnit(this, 5, pos)
        uid
      case Move(uid, dir) =>
        val u = units(uid)
        val newpos = coordsys.move(u.pos, dir)
        if (newpos != u.pos) {
          findByPos(newpos) match {
            case Some((tuid, _)) => attack(uid, tuid)
            case None => u.pos = newpos
          }
        }
      case Suicide(uid) => ()
    }
  }
  def findById(id: Int): LcUnit = units(id)
  def findByPos(pos: Coord): Option[(Uid, LcUnit)] =
    units.find(kv => kv._2.pos == pos && kv._2.isAlive)
  def attack(from: Uid, to: Uid) {
    val fu = units(from)
    val tu = units(to)
    fu.hp -= 1
    tu.hp -= 1
    if (tu.isDead && fu.isAlive)
      fu.pos = tu.pos
  }
  def fieldOfView(me: Uid): Map[Uid, LcUnit] =
    units filter { case (uid, u) =>
      val pos = units(me).pos
      val sight = units(me).sight
      uid == me || (u.isAlive && coordsys.distance(u.pos, pos) <= sight)
    }
}

final class LcUnit(private val game: Game, var hp: Int, var pos: (Int, Int)) {
  val sight = 4
  def isDead = hp <= 0
  def isAlive = !isDead
}
