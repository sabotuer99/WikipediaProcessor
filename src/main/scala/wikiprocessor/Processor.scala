package wikiprocessor

import java.util.regex.Pattern

import scala.collection.mutable
import scala.xml.pull.XMLEventReader
import scala.io.Source
import scala.xml.pull._

class Processor {





}

object Processor {
  def clean(raw: String): String = {
    val eraserRegex = List(
      "</?blockquote>",
      "<math>.*?</math>",
      "<ref[^\\/\\>]*>.*?((</ref>)|(</>))",
      "<ref[^\\/\\>]*?/>",
      "</ref>",
      "<ref name=>.{1,50}</>",
      "<ref name=[^\\/\\>]*?>",
      "<#REDIRECT.*?>",
      "== Sources ==.*",
      "== References ==.*",
      "=?==.*?===?",
      "\\{\\{cite .*?\\}\\}",
      "\\{\\{.*?\\}\\}:?",
      "'''?",
      "\\[\\[File:.*?\n",
      "\\[\\[Category:.*?\\]\\]",
      "\\[{2}[^|\\]]+[|]",
      "\\.\\.\\.",
      "\\[https?:.*?\\]",
      "<!--.*?-->",
      "<\n\\|.*?>"
    )

    raw.
      replaceAll("(?s)(" + eraserRegex.mkString(")|(") + ")" , "").
      replaceAll("nbsp;", " ").
      replaceAll("[^0-9A-Za-z\\- \n.']", " ").
      replaceAll(" +", " ").
      replaceAll("\n+", "\n").
      trim
  }

  def main(args: Array[String]) {
    val in = Source.fromFile("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.xml")
    val out_file = new java.io.FileOutputStream("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.cleaned.txt")
    val out_stream = new java.io.PrintStream(out_file)

    val eventReader = new XMLEventReader(in)
    var line = 0

    val segmenter = new Segmenter("/home/troy/Downloads/Wikipedia Data/bigrams/enwiki-20190301-pages-articles.bigrams", 100)



    def nextLine: XMLEvent = if (eventReader.hasNext) eventReader.next() else null

    def getUntilEndTag(tagName: String): String = nextLine match {
      case null => ""
      case EvElemEnd(_, tag) if tag == tagName => ""
      case EvText(text: String) => "<" + text + ">" + getUntilEndTag(tagName)
      case _ => getUntilEndTag(tagName)
    }

    while (eventReader.hasNext) {
      eventReader.next() match {
        case EvElemStart(_, "text", _, _) => {
          val rawText = getUntilEndTag("text")
          val allText = clean(rawText)

          line += 1
        }
        case _ => { line += 1 }
      }
    }
    segmenter.closeAll()
  }
}
