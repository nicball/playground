package landcraft.model

case class UnitAttribute(
    name: String,
    team: Team,
    hitPoint: Int,
    movePoint: Int,
    visionRadius: Int,
    attackRadius: Int,
    attackPoint: Int,
    attackDamage: Int
)

class OutOfMove extends Throwable

class Unit(
    val position: Map#Coord,
    val attribute: UnitAttribute,
    val state: UnitAttribute
) {
    def dealDamage = (attribute.attackDamage, new Unit(
        position, attribute, state.copy(attackPoint = state.attackPoint - 1)))
    def takeDamage(damage: Int) = new Unit(
        position,
        attribute,
        state.copy(
            hitPoint = state.hitPoint - damage
        )
    )
    def canAttack = state.attackPoint > 0
    def dead = state.hitPoint <= 0
    def move(dir: Direction.Value) =
        if (state.movePoint < 1) throw new OutOfMove
        else new Unit(position move dir, attribute, state.copy(movePoint = state.movePoint - 1))
    def reset = new Unit(position, attribute, attribute)
}

object Unit {
    def marineAttribute(team: Team) = UnitAttribute(
        name = "marine",
        team = team,
        hitPoint = 15,
        movePoint = 2,
        visionRadius = 5,
        attackRadius = 4,
        attackPoint = 2,
        attackDamage = 5
    )
    def marine(team: Team, pos: Map#Coord) = {
        val attr = marineAttribute(team)
        new Unit(pos, attr, attr)
    }
}
