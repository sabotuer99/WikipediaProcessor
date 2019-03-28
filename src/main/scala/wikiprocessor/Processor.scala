package wikiprocessor

import java.util

import scala.io.Source
import scala.xml.pull.{XMLEventReader, _}


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
          case (_, _, 0) => diff match {
            case pos if pos > 0 => removeBalancedTagIter(body.substring(nextOpen + beginTag.length), acc +
              body.substring(0, nextOpen), 1)
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

  val processorCount: Int = Runtime.getRuntime.availableProcessors()
  val rawArticleQueue: util.concurrent.BlockingDeque[String] =
    new java.util.concurrent.LinkedBlockingDeque[String](processorCount * 100)
  val bigramQueue: util.concurrent.BlockingDeque[String] =
    new java.util.concurrent.LinkedBlockingDeque[String](processorCount * 25000)
  val cleanArticleQueue: util.concurrent.BlockingDeque[String] =
    new java.util.concurrent.LinkedBlockingDeque[String](processorCount * 100)

  def process(input: String): Unit = {
    val in = Source.fromFile(input)
    val baseFolder = input.replaceAll("[^/]*$", "")
    val baseFile = input.replaceAll("^.*[/]", "").replace(".xml", "")
    val out_file = new java.io.FileOutputStream(baseFolder + "/" + baseFile + ".cleaned.txt")
    val out_stream = new java.io.PrintStream(out_file)
    val segmenter = new Segmenter(baseFolder + "/bigrams/" + baseFile + "." + "bigrams", 100)
    val eventReader = new XMLEventReader(in)

    // start all the Runnables
    val rawArticleProcessors = 0 until processorCount map (_ => new RawArticleProcessor())
    val bigramWriter = new BigramWriter(segmenter)
    val cleanTextWriter = new CleanTextWriter(out_stream)
    val threads = (rawArticleProcessors ++ List(bigramWriter, cleanTextWriter)).map(runner => new Thread(runner))
    threads.foreach(t => t.start())

    var article = 0

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
          rawArticleQueue.put(rawText)

          article += 1
          if (article == 1) print("\n" + java.time.Instant.now() + " [Start]")
          else if (article % 1000 == 0) {
            print("\n" + java.time.Instant.now() + " [" + article + "]")
          }
          else if (article % 50 == 0) print(".")
        }
        case _ =>
      }
    }

    // wait for the queues to drain
    while (bigramQueue.size + cleanArticleQueue.size + rawArticleQueue.size > 0 ||
      !bigramWriter.isWaiting ||
      !cleanTextWriter.isWaiting ||
      !rawArticleProcessors.forall(p => p.isWaiting)) {
      Thread.sleep(1000)
    }

    println("\nClosing segmenter...")
    segmenter.closeAll()
    println("Closing cleaned text file...")
    out_stream.close()
    println("Closing threads...")
    threads.foreach(t => t.interrupt())
  }

  def main(args: Array[String]) {
    process("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.xml")
    //process("/home/troy/Downloads/Wikipedia Data/short.xml")
  }

  class RawArticleProcessor() extends Runnable {

    var isWaiting: Boolean = true

    override def run(): Unit = {
      try {
        while (true) {
          isWaiting = true
          val rawText = rawArticleQueue.take()
          isWaiting = false
          val cleaned = clean(rawText)
          val tokens = cleaned.
            replace("\n", " ").
            replace(".", "").
            toLowerCase.split(" ", -1)

          cleanArticleQueue.put(cleaned)

          for ((a, b) <- tokens zip tokens.drop(1) if a.nonEmpty && b.nonEmpty) {
            val bigram = a + " " + b
            bigramQueue.put(bigram)
          }
        }
      } catch {
        case _: Exception =>
      }
    }
  }

  class BigramWriter(segmenter: Segmenter) extends Runnable {
    var isWaiting = true

    override def run(): Unit = {
      try {
        while (true) {
          isWaiting = true
          val bigram = bigramQueue.take()
          isWaiting = false
          segmenter.write(bigram)
        }
      } catch {
        case _: Exception =>
      }
    }
  }

  class CleanTextWriter(out_stream: java.io.PrintStream) extends Runnable {

    var isWaiting = true

    override def run(): Unit = {
      try {
        while (true) {
          isWaiting = true
          val cleaned = cleanArticleQueue.take()
          isWaiting = false
          out_stream.println(cleaned)
          out_stream.flush()
        }
      } catch {
        case _: Exception =>
      }
    }
  }

}
