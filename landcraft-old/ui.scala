package ui

import model._

class Window {
  def render(game: Game) {
    val units = game.myUid match {
      case Some(me) => game fieldOfView me
      case None => game.units filter { _._2.isAlive }
    }
    val size = game.coordsys.size
    val screen = Array.fill(size, size)("#")
    for ((uid, unit) <- units) {
      val sym =
        if (game.myUid.isDefined && uid == game.myUid.get) 'M'
        else 'U'
      val color =
        if (unit.hp >= 5) io.AnsiColor.GREEN
        else if (unit.hp >= 3) io.AnsiColor.YELLOW
        else io.AnsiColor.RED
      screen(unit.pos._1)(unit.pos._2) = s"${color}${sym}${io.AnsiColor.RESET}"
    }
    for {
      j <- size - 1 to 0 by -1
      i <- 0 to size - 1
    } {
      print(screen(i)(j))
      if (i == size - 1) print('\n')
    }
  }
  def getAction(game: Game): Program[Unit] = {
    io.StdIn.readChar() match {
      case 'w' | 'W' => Program.move(game.myUid.get, Up)
      case 's' | 'S' => Program.move(game.myUid.get, Down)
      case 'a' | 'A' => Program.move(game.myUid.get, Left)
      case 'd' | 'D' => Program.move(game.myUid.get, Right)
      case 'q' | 'Q' => Program.suicide(game.myUid.get)
      case _ => getAction(game)
    }
  }
}
