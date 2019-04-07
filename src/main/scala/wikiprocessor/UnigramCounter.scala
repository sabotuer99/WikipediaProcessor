package wikiprocessor

import scala.collection.mutable
import scala.io.Source

object UnigramCounter {

  def main(args: Array[String]): Unit = {
    val in = Source.fromFile("/home/troy/Downloads/Wikipedia Data/enwiki-20190301-pages-articles.cleaned.second_attempt.txt")
    var lineNum = 0
    val unigramCounts: mutable.HashMap[String, Int] = mutable.HashMap()
    for(line <- in.getLines()){
      lineNum += 1
      if(lineNum % 2000000 == 0) println("[" + lineNum + "] Size: " + unigramCounts.size)
      else if(lineNum % 100000 == 0) print(".")
      for(word <- line.toLowerCase.replace(".", "").split(" ", -1)){
        unigramCounts.put(word, unigramCounts.withDefault(_ => 0)(word) + 1)
      }
    }

    // write unigram counts
    val out_file = new java.io.FileOutputStream("/home/troy/Downloads/Wikipedia Data/unigrams/enwiki-20190301-pages-articles.unigrams.counts.txt")
    val unigram_stream = new java.io.PrintStream(out_file)
    unigramCounts.filter(_._2 > 1).foreach(entry => unigram_stream.println(entry._1 + " " + entry._2))
    unigram_stream.println(unigramCounts.values.sum)
    unigram_stream.close
  }
}
