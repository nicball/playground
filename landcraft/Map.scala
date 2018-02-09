package landcraft.model

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
    def move(dir: Direction.Value) = dir match {
        case Direction.Up => Coord(x, y + 1)
        case Direction.Down => Coord(x, y - 1)
        case Direction.Left => Coord(x - 1, y)
        case Direction.Right => Coord(x + 1, y)
    }
}

class InvalidCoord

class Map(val size: Int) {
    def visionOf(unit: Unit) = {
        val result = collection.mutable.Set[Coord]()
        val toExplore = collection.mutable.Queue[Coord](unit.position)
        while (toExplore.isEmpty) {
            val c = toExplore.dequeue()
            result += c
            for (dir <- Direction.values) {
                val nc = c move dir
                if (!result(nc) && nc visibleFrom (unit.position, unit.visionRadius))
                    toExplore.enqueue(nc)
            }
        }
        result
    }
    def attackRangeOf(unit: Unit) = {
        val result = collection.mutable.Set[Coord]()
        val toExplore = collection.mutable.Queue[Coord](unit.position)
        while (toExplore.isEmpty) {
            val c = toExplore.dequeue()
            result += c
            for (dir <- Direction.values) {
                val nc = c move dir
                if (!result(nc) && nc visibleFrom (unit.position, unit.attackRadius))
                    toExplore.enqueue(nc)
            }
        }
        result
    }
}

object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
}