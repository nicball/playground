package network

sealed trait Message
case class Join(name: String) extends Message
case class JoinResponse(uid: Uid) extends Message
case object YourTurn extends Message
case class GameCommand(cmd: Command[Any]) extends Message

trait Serial[A] {
  def read(is: DataInputStream): A
  def write(os: DataOutputStream, a: A)
}

object Serial {
  implicit val serialCommand = new Serial[Command[Any]] {
    def read(is: DataInputStream): Command[Any] = {
      val ev = implicitly[Serial[Dir]]
      is.readByte() match {
        case 1 => Spawn(is.readInt(), is.readInt())
        case 2 => Move(ev.read(is))
        case 3 => Quit(is.readInt())
      }
    }
    def write(os: DataOutputStream, cmd: Command[Any])(implicit ev: Serial[Dir]) {
      val ev = implicitly[Serial[Dir]]
      cmd match {
        case Spawn(x, y) =>
          os.writeByte(1)
          os.writeInt(x)
          os.writeInt(y)
        case Move(dir) =>
          os.writeByte(2)
          ev.write(os, dir)
        case Quit(uid) =>
          os.writeByte(3)
          os.writeInt(uid)
      }
    }
  }
  implicit val serialDir = new Serial[Dir] {
    def read(os: DataInputStream) =
      os.readByte() match {
        case 1 => Up
        case 2 => Down
        case 3 => Left
        case 4 => Right
      }
    def write(os: DataOutputStream, dir: Dir) {
      os.writeByte(dir match {
        case Up => 1
        case Down => 2
        case Left => 3
        case Right => 4
      })
    }
  }
}
