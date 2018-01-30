package landcraft.simulator

sealed abstract class Action
case class Spawn(unit: Map#Unit) extends Action
case class Attack(from: Map#Unit, to: Map#Unit) extends Action
case class Move(unit: Map#Unit, pos: Map#Coord) extends Action