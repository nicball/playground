package object util {
  implicit class Function1Ops[A, B](value: A => B) {
    def map[C](f: B => C): A => C = { a => f(value(a)) }
    def flatMap[C](f: B => A => C): A => C = { a => f(value(a))(a) }
  }
}

package util {

class EventBus[E] {
  private val subscribers = collection.mutable.Set[E => Unit]()
  def subscribe(f: E => Unit) { subscribers += f }
  def unsubscribe(f: E => Unit) { subscribers -= f }
  def fire(e: E) {
    for (s <- subscribers) s(e)
  }
}

}
