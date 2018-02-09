package landcraft.render

import landcraft.model._

class Canvas(size: Int) {
    val data = new Array[Short](size * size * 3)
    class Pixel(x: Int, y: Int) {
        private val base = (x * size + y) * 3
        def r = data(base + 0)
        def r_=(v: Short) { data(base + 0) = v }
        def g = data(base + 1)
        def g_=(v: Short) { data(base + 1) = v }
        def b = data(base + 2)
        def b_=(v: Short) { data(base + 2) = v }
    }
    def apply(x: Int, y: Int) = new Pixel(x, y)
    def update(x: Int, y: Int, rgb: (Short, Short, Short)) {
        val p = this(x, y)
        p.r = rgb._1
        p.g = rgb._2
        p.b = rgb._3
    }
    def line(a: (Int, Int), b: (Int, Int)) {
        
    }
}

package object Render {
    val canvas_size = 500
    def render(map: Map, units: Set[Unit]) {
        val canvas = new Canvas(canvas_size)

    }
}