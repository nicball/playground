package landcraft.model

class InvalidMove

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

class Unit(
    val position: Map#Coord,
    val attribute: UnitAttribute,
    val state: UnitAttribute
) {
    def dealDamage = (attribute.attackDamage, new Unit(
        position, attribute, state.copy(attackPoint = state.attackPoint - 1))
    def takeDamage(damage: Int) = new Unit(
        position,
        attribute,
        state.copy(
            hitPoint = state.hitPoint - damage
        )
    )
    def dead = state.hitPoint <= 0
    def move(dir: Direction.Value) = {
        val target = position move dir
        if (state.movePoint < 1) throw new InvalidMove
        if (target movableFrom position)
            new Unit(target, attribute, state.copy(movePoint = state.movePoint - 1))
        else
            throw new InvalidCoord
    }
    def reset = new Unit(position, attribute, attribute)
}