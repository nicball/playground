package example

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._

object Hello {
  val sep = "\n%%\n"

  def getBuildTableRows(url: String): Stream[Element] = {
    val doc = JsoupBrowser().get(url)
    val rows = doc >> elementList("table tr")
    doc >?> element(".pull-right") >> attr("href") match {
      case None => rows.toStream
      case Some(link) => rows.toStream #::: getBuildTableRows(url + link)
    }
  }

  def splitBy[A](xs: Iterable[A], pred: A => Boolean): List[List[A]] =
    (xs :\ List(List[A]())) { case (x, (acc :: rest)) =>
      if (!pred(x)) (x :: acc) :: rest
      else Nil :: acc :: rest
    }

  def rowToBuildOrder(hostUrl: String)(row: Element): BuildOrder = {
    val link :: title :: races :: cat :: _ = row >> elementList("td")
    System.err.println("Fetching " + (title >> text("b")))
    val specDoc = JsoupBrowser().get(hostUrl + (link >> attr("href")("a")))
    val body = (specDoc >> element("div.col-md-8"))
    val blocks = splitBy[Element](body.children, x => x.tagName == "h3" && x.hasAttr("id"))
    val build = body >> element("#build-1")
    val desc = blocks(1)
    try {
      val anal = blocks(4)
      BuildOrder(
        title >> text("b"),
        races >> text,
        cat >> text,
        desc.map(_.outerHtml).mkString,
        build.outerHtml,
        anal.map(_.outerHtml).mkString
      )
    }
    catch {
      case e: IndexOutOfBoundsException =>
        BuildOrder(
          title >> text("b"),
          races >> text,
          cat >> text,
          desc.map(_.outerHtml).mkString,
          build.outerHtml,
          ""
        )
    }
  }

  def main(args: Array[String]): Unit = {
    val out = new java.io.PrintWriter("/home/sahib/bo.txt")
    try {
      getBuildTableRows("http://lotv.spawningtool.com/build/")
        .map(rowToBuildOrder("http://lotv.spawningtool.com/"))
        .foreach{ case BuildOrder(t, r, c, d, b, a) =>
          out.print(t)
          out.print(sep)
          out.print(r)
          out.print(sep)
          out.print(c)
          out.print(sep)
          out.print(d)
          out.print(sep)
          out.print(b)
          out.print(sep)
          out.print(a)
          out.print(sep)
        }
    }
    finally {
      out.close()
    }
  }
}

case class BuildOrder(
  title: String,
  races: String,
  category: String,
  description: String,
  buildReplay: String,
  analysis: String
)
