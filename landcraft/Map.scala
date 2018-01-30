package landcraft.model

class InvalidCoord
class InvalidMove

class PlainMap(val size: Int) {
    case class Coord(x: Int, y: Int) {
        if (x < 0 || x >= size || y < 0 || y >= size)
            throw new InvalidCoord

        def distance(other: Coord) =
            Math.sqrt((x - other.x) * (x - other.x)
                + (y - other.y) * (y - other.y)).asInstanceOf[Int]
        def movableFrom(from: Coord, mp: Int = 1) =
            visibleFrom(from, mp)
        def visibleFrom(from: Coord, vr: Int) =
            distance(from) <= vr
        def move(dir: Direction) = dir match {
            case Direction.Up => Coord(x, y + 1)
            case Direction.Down => Coord(x, y - 1)
            case Direction.Left => Coord(x - 1, y)
            case Direction.Right => Coord(x + 1, y)
        }
    }

    object Direction extends Enumeration {
        type Direction = Value
        val Up, Down, Left, Right = Value
    }

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
        val position: Coord,
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
        def move(dir: Direction) = {
            val target = position move dir
            if (state.movePoint < 1) throw new InvalidMove
            if (target movableFrom position)
                new Unit(target, attribute, state.copy(movePoint = state.movePoint - 1))
            else
                throw new InvalidCoord
        }
        def reset = new Unit(position, attribute, attribute)
    }
}