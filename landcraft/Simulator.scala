package landcraft.simulate

import landcraft.model._

class OutOfAttackPoint extends Throwable

class Simulator(val map: Map) {
    var units = Set[Map#Unit]()
    def spawn_unit(u: Map#Unit) { units += u }
    def move_unit(u: Map#Unit, dir: Direction.Value) {
        units -= u
        units += u move dir
    }
    def attack(from: Map#Unit, to: Map#Unit) {
        if (!from.can_attack) throw new OutOfAttackPoint
        val (dmg, new_from) = from.deal_damage
        val new_to = to take_damage dmg
        units -= from
        units += new_from
        units -= to
        if (!new_to.dead) units += new_to
    }
    def end_turn() { units = units map (_.reset) }
}

object Simulator {
    val team = new Team((0, 0, 0))
    val map = new Map(64)
    val marine = map.Unit.marine(team, map.Coord(30, 30))
    val sim = new Simulator(map)
    sim.spawn_unit(marine)
    sim.move_unit(marine, Direction.Up)
    sim.attack(marine, marine)
    sim.end_turn()
}
