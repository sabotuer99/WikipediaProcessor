package wikiprocessor

import java.util.regex.Pattern

import scala.collection.mutable
import scala.xml.pull.XMLEventReader
import scala.io.Source
import scala.xml.pull._

class Processor {


}

object Processor {

  def removeBalancedTag(beginTag: String, endTag: String, text: String): String = {
    def removeBalancedTagIter(body: String, acc: String, depth: Int): String =
      if (body == "") acc
      else {
        val nextOpen = body.indexOf(beginTag)
        val nextClose = body.indexOf(endTag)
        val diff = nextClose - nextOpen
        (nextOpen, nextClose, depth) match {
          // no more tags
          case (open, close, _) if open == close => acc + body
          // zero depth with endTag first is invalid
          case (-1, close, 0) if close > -1 => acc + body
          // first opening tag
          case(_, _, 0) => diff match {
            case pos if pos > 0 => removeBalancedTagIter(body.substring(nextOpen + beginTag.length), acc +
              body.substring(0,nextOpen), 1)
            // if depth is zero and we encounter an end tag first, just return what we have, not well formed
            case error if error < 0 => acc + body
          }
          // begin tag first, advance cursor and increase depth
          case (open, close, i) if close > open && i > 0 =>
            removeBalancedTagIter(body.substring(nextOpen + beginTag.length), acc, depth + 1)
          // end tag first, advance cursor and decrease depth
          case (open, close, i) if open > close && i > 0 =>
            removeBalancedTagIter(body.substring(nextClose + endTag.length), acc, depth - 1)
        }
      }

    removeBalancedTagIter(text, "", 0)
  }

  def clean(raw: String): String = {
    val eraserRegex = List(
      "</?blockquote>",
      "<math>.*?</math>",
      "\\{\\| class=\"[^\"]*wikitable[^\"]*\".*?\\|\\}",
      "<ref[^\\/\\>]*>.*?((</ref>)|(</>))",
      "<ref[^\\/\\>]*?/>",
      "</ref>",
      "<ref name=>.{1,50}</>",
      "<ref name=[^\\/\\>]*?>",
      "<#REDIRECT.*?>",
      "== Sources ==.*",
      "== References ==.*",
      "=?==.*?===?",
      "'''?",
      "\\[\\[File:.*?\n",
      "\\[\\[Category:.*?\\]\\]",
      "\\[{2}[^|\\]]+[|]",
      "\\.\\.\\.",
      "\\[https?:.*?\\]",
      "<!--.*?-->",
      "<\n\\|.*?>"
    )

    removeBalancedTag("{{", "}}", raw).
      replaceAll("(?s)(" + eraserRegex.mkString(")|(") + ")", "").
      replaceAll("nbsp;", " ").
      replaceAll("[^0-9A-Za-z\\- \n.']", " ").
      replaceAll("\n+", "\n").
      // second pass trim, eliminate leading/trailing quote and dash, and stand alone numbers
      replaceAll("((?<=[\n\\s])')|('(?=[\n\\s]))|(--+)|((?=[\n\\s])-)|(-(?=[\n\\s]))|(\\b-?[0-9]+\\b)", "").
      replaceAll(" - ", " ").
      replaceAll(" +", " ").
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
          val tokens = cleaned.
            replace("\n", " ").
            replace(".", "").
            toLowerCase.split(" ", -1)

          article += 1
          if (article == 1) print("\n" + java.time.Instant.now() + " [Start]")
          else if (article % 1000 == 0) print("\n" + java.time.Instant.now() + " [" + article + "]")
          else if (article % 50 == 0) print(".")

          for ((a, b) <- tokens zip tokens.drop(1) if a.nonEmpty && b.nonEmpty) {
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
