package wikiprocessor

import java.util.regex.Pattern

import scala.collection.mutable
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

    val wordCounts: mutable.HashMap[String, Int] = mutable.HashMap()
    val bigramCounts: mutable.HashMap[String, Int] = mutable.HashMap()

    val eventReader = new XMLEventReader(in)
    var line = 0
    var lastCheckpoint = 0
    var bigram_epoch = 0

    val segmenter = new Segmenter("/home/troy/Downloads/Wikipedia Data/bigrams/enwiki-20190301-pages-articles.bigrams", 100)

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

//    def writeLineCounts = {
//      val counts_file = new java.io.FileOutputStream("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.counts.txt")
//      val counts_stream = new java.io.PrintStream(counts_file)
//      val word_count = wordCounts.values.sum
//      wordCounts.filter(_._2 > 1).foreach(entry => counts_stream.println(entry._1 + " " + entry._2))
//      counts_stream.print(word_count)
//      counts_stream.close
//      println("Checkpoint at line " + line + ", word count " + word_count)
//      word_count
//    }
//
//    def writeBigrams = {
//      println("Dumping bigrams after line " + line)
//      val bigram_file = new java.io.FileOutputStream("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.bigrams" + bigram_epoch + ".txt")
//      val bigram_stream = new java.io.PrintStream(bigram_file)
//      bigramCounts.filter(_._2 > 1).foreach(entry => bigram_stream.println(entry._1 + " " + entry._2))
//      bigram_stream.print(bigramCounts.values.sum)
//      bigram_stream.close
//      bigramCounts.clear()
//      bigram_epoch += 1
//    }

    while (eventReader.hasNext) {
      eventReader.next() match {
        case EvElemStart(_,"text",_,_) => {
          // Process page element
          var keepProcessing = true

//          if(line - lastCheckpoint > 10000000){
//            val word_count: Int = writeLineCounts
//
//            // Every 250000000 words, dump and reset the bigram hashmap
//            // This is to avoid OutOfMemoryException
//            if(word_count > 250000000 * (bigram_epoch + 1)){
//              writeBigrams
//            }
//
//            lastCheckpoint = line
//          }

          while (eventReader.hasNext && keepProcessing){
            eventReader.next() match {
              case EvElemEnd(_,"text") => {
                out_stream.print("\n")
                keepProcessing = false
              }
              case EvText(text: String) if ignoredTagRange.exists(text.startsWith) => {
                find({ case EvText(stop) if ignoredTagRange.exists(x => stop.startsWith("/" + x)) => true; case _ => false })
                out_stream.print(" ")
              }
              case EvText(text: String) if ignoredContains.exists(text.contains) => out_stream.print(" ")
              case EvText(text: String) if ignoredExact.contains(text) => out_stream.print(" ")
              case EvText(text: String) if ignoredPrefix.exists(x => text.startsWith(x)) => out_stream.print(" ")
              case EvText(text) => {
                line += 1
                val cleaned = text.
                  replaceAll("(\\[\\[)|((|.*)?\\]\\])|(\\{\\{.*?\\}\\})|(===?=?)|('''?)|(\\n)|(nbsp;)", " ").
                  toLowerCase.replaceAll("[^a-z0-9'\\- .]", " ")

                val trimmed = cleaned.
                  replaceAll("[.]", " ").
                  replaceAll("[0-9]","").
                  split(" ", -1)

//                // count individual words
//                trimmed.
//                  map(word => if (word.nonEmpty && word != " ") wordCounts.put(word, wordCounts.withDefault(_ => 0)(word) + 1))
//
//
                // segment bigrams
                for((a, b) <- trimmed zip trimmed.drop(1) if a.nonEmpty && b.nonEmpty) {
                  val bigram = a + " " + b
                  //bigramCounts.put(bigram, bigramCounts.withDefault(_ => 0)(bigram) + 1)
                  segmenter.write(bigram)
                }

                out_stream.println(cleaned)
                //println(line + ":" + cleaned)
              }
              case _ => {}
            }
          }
        }
        case _ => {}
      }
    }
//    writeLineCounts
//    writeBigrams
    out_stream.close
    segmenter.closeAll
  }
}
