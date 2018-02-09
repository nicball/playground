package landcraft.model


class Map(val size: Int) {
    class InvalidCoord extends Throwable
    case class Coord(x: Int, y: Int) {
        if (x < 0 || x >= size || y < 0 || y >= size)
          throw new InvalidCoord
        def distance(other: Coord) =
            Math.sqrt((x - other.x) * (x - other.x)
                + (y - other.y) * (y - other.y)).asInstanceOf[Int]
        def movable_from(from: Coord, mp: Int = 1) =
            visible_from(from, mp)
        def visible_from(from: Coord, vr: Int) =
            distance(from) <= vr
        def move(dir: Direction.Value) = dir match {
            case Direction.Up => Coord(x, y + 1)
            case Direction.Down => Coord(x, y - 1)
            case Direction.Left => Coord(x - 1, y)
            case Direction.Right => Coord(x + 1, y)
        }
    }

    case class UnitAttribute(
        name: String,
        team: Team,
        hit_point: Int,
        move_point: Int,
        vision_radius: Int,
        attack_radius: Int,
        attack_point: Int,
        attack_damage: Int
    )

    class OutOfMovePoint extends Throwable

    class Unit(
        val position: Coord,
        val attribute: UnitAttribute,
        val state: UnitAttribute
    ) {
        def deal_damage = (attribute.attack_damage, new Unit(
            position, attribute, state.copy(attack_point = state.attack_point - 1)))
        def take_damage(damage: Int) = new Unit(
            position,
            attribute,
            state.copy(
                hit_point = state.hit_point - damage
            )
        )
        def can_attack = state.attack_point > 0
        def dead = state.hit_point <= 0
        def move(dir: Direction.Value) =
            if (state.move_point < 1) throw new OutOfMovePoint
            else new Unit(position move dir, attribute, state.copy(move_point = state.move_point - 1))
        def reset = new Unit(position, attribute, attribute)
    }

    object Unit {
        def marine_attribute(team: Team) = UnitAttribute(
            name = "marine",
            team = team,
            hit_point = 15,
            move_point = 2,
            vision_radius = 5,
            attack_radius = 4,
            attack_point = 2,
            attack_damage = 5
        )
        def marine(team: Team, pos: Coord) = {
            val attr = marine_attribute(team)
            new Unit(pos, attr, attr)
        }
    }

    def vision_of(unit: Unit) = {
        val result = collection.mutable.Set[Coord]()
        val to_explore = collection.mutable.Queue[Coord](unit.position)
        while (to_explore.isEmpty) {
            val c = to_explore.dequeue()
            result += c
            for (dir <- Direction.values) {
                val nc = c move dir
                if (!result(nc) && (nc visible_from (unit.position, unit.state.vision_radius)))
                    to_explore.enqueue(nc)
            }
        }
        result
    }
    def attack_range_of(unit: Unit) = {
        val result = collection.mutable.Set[Coord]()
        val to_explore = collection.mutable.Queue[Coord](unit.position)
        while (to_explore.isEmpty) {
            val c = to_explore.dequeue()
            result += c
            for (dir <- Direction.values) {
                val nc = c move dir
                if (!result(nc) && (nc visible_from (unit.position, unit.state.attack_radius)))
                    to_explore.enqueue(nc)
            }
        }
        result
    }
}

object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
}
