package wikiprocessor

import java.util.regex.Pattern

import scala.xml.pull.XMLEventReader
import scala.io.Source
import scala.xml.pull._

class Processor {





}

object Processor {
  def main(args: Array[String]) {
    val in = Source.fromFile("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.xml")
    val out_file = new java.io.FileOutputStream("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.cleaned.txt")
    val out_stream = new java.io.PrintStream(out_file)

    val eventReader = new XMLEventReader(in)
    var line = 0

    val ignoredTagRange = List("ref", "math", "sub", "sup", "wikitable", "center", "small", "div")
    val ignoredContains = List("==External links==", "==See also==", "==References==", "==Works cited==", "{{Navboxes", "{{Infobox", "|", "text-align:", "background:#", "style=")
    val ignoredExact = List("br", "br/", "br /", "blockquote", "/blockquote")
    val ignoredPrefix = List("!--")

    def find(p: XMLEvent => Boolean): Option[XMLEvent] = {
      if (eventReader.hasNext) {
        val xml:XMLEvent = eventReader.next()
        line += 1
        if (p(xml)) Some(xml) else find(p)
      }
      else None
    }

    while (eventReader.hasNext) {
      eventReader.next() match {
        case EvElemStart(_,"text",_,_) => {
          // Process page element
          var keepProcessing = true
          while (eventReader.hasNext && keepProcessing){
            eventReader.next() match {
              case EvElemEnd(_,"text") => keepProcessing = false
              case EvText(text: String) if ignoredTagRange.exists(text.startsWith) =>
                find({case EvText(stop) if ignoredTagRange.exists(x => stop.startsWith("/" + x)) => true; case _ => false})
              case EvText(text: String) if ignoredContains.exists(text.contains) => {}
              case EvText(text: String) if ignoredExact.contains(text) => {}
              case EvText(text: String) if ignoredPrefix.exists(x => text.startsWith(x)) => {}
              case EvText(text) => {
                line += 1
                val cleaned = text.
                  replaceAll("(\\[\\[)|((|.*)?\\]\\])|(\\{\\{.*?\\}\\})|(===?=?)|('''?)|(\\n)|(nbsp;)", "")//.
                  //toLowerCase.replaceAll("[^a-z0-9'\\- ]", "")
                out_stream.print(cleaned)
                //println(line + ":" + cleaned)
              }
              case _ => {}
            }
          }
        }
        case _ => {}
      }
    }

    out_stream.close
  }
}
