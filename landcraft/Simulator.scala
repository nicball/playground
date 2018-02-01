package landcraft.simulate

import landcraft.model._

class OutOfAttackPoint

type UnitId = Int
class Simulator(val map: Map) {
    var units = Set[Unit]()
    def spawnUnit(u: Unit) = units += u
    def moveUnit(u: Unit, dir: Direction) = {
        units -= u
        units += u move dir
    }
    def attack(from: Unit, to: Unit) = {
        if (!from.canAttack) throw new OutOfAttackPoint
        val (dmg, newFrom) = from.dealDamage
        val newTo = to takeDamage dmg
        units -= from
        units += newFrom
        units -= to
        if (!newTo.dead) units += newTo
    }
}