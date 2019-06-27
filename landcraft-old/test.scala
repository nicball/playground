package test

import model._
import ui._
import model.Program._

object Test {
  def main(args: Array[String]) {
    val win = new Window
    val game = new Game(new CoordSys(10))
    game.myUid = Some(game.run(
      for {
        me <- spawn(5, 0)
        enemy <- spawn(5, 9)
        _ <- move(enemy, Up)
        _ <- move(enemy, Down)
        _ <- move(enemy, Left)
      } yield me
    ))
    game.findById(game.myUid.get).hp += 1 // Cheat!
    win.render(game)
    while (true) {
      val act = win.getAction(game)
      game.run(act)
      win.render(game)
      if (isSuicide(act)) {
        game.history foreach println _
        sys.exit()
      }
    }
  }
}
