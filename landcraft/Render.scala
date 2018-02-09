package landcraft.render

import landcraft.model._

private class Canvas(size: Int) {
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
    def vert_line(x0: Int, x1: Int, y: Int, color: (Short, Short, Short)) {
        for (i <- x0 to x1)
          this(i, y) = color
    }
    def hori_line(x: Int, y0: Int, y1: Int, color: (Short, Short, Short)) {
        for (i <- y0 to y1)
          this(x, i) = color
    }
}

package object Render {
    val canvas_size = 500
    val margin_size = 5
    val gridline_color = (255, 255, 255) : (Short, Short, Short)
    def render(map: Map, units: Set[Unit]) {
        val canvas = new Canvas(canvas_size)
        val grid_size = (canvas_size - 2 * margin_size) / map.size
        val map_to_canvas = (m: (Int, Int)) => {
            val f = (x: Int) => margin_size + x * grid_size
            (f(m._1), f(m._2))
        }
        for (i <- margin_size to (canvas_size - margin_size) by grid_size) {
            canvas.vert_line(margin_size, canvas_size - margin_size, i, gridline_color)
            canvas.hori_line(i, margin_size, canvas_size - margin_size, gridline_color)
        }

    }
}
