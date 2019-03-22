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
    var article = 0

    val segmenter = new Segmenter("/home/troy/Downloads/Wikipedia Data/bigrams/enwiki-20190301-pages-articles.bigrams", 100)



    def nextLine: XMLEvent = if (eventReader.hasNext) eventReader.next() else null

    def getUntilEndTag(tagName: String, prepend: String): String = nextLine match {
      case null => prepend
      case EvElemEnd(_, tag) if tag == tagName => prepend
      case EvText(text: String) => getUntilEndTag(tagName, prepend + "<" + text + ">")
      case _ => getUntilEndTag(tagName, prepend)
    }

    while (eventReader.hasNext) {

      eventReader.next() match {
        case EvElemStart(_, "text", _, _) => {
          val rawText = getUntilEndTag("text", "")
          val cleaned = clean(rawText)
          val tokens = cleaned.replace("\n", " ").toLowerCase.split(" ", -1)

          article += 1
          if (article % 1000 == 0) print("\n" + java.time.Instant.now() + " [" + article + "]" )
          else if (article % 50 == 0) print(".")

          for((a, b) <- tokens zip tokens.drop(1) if a.nonEmpty && b.nonEmpty) {
            val bigram = a + " " + b
            segmenter.write(bigram)
          }

          out_stream.println(cleaned)
          out_stream.flush()
        }
        case _ =>
      }
    }
    segmenter.closeAll()
  }
}
