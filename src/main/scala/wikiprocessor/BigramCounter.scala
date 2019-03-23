package wikiprocessor

import java.util.Scanner

import scala.collection.mutable
import scala.io.Source

object BigramCounter {

  def main(args: Array[String]) {
    for( i <- 0 until 100){
      print("\nProcessing file #" + i)
      val in = Source.fromFile("/home/troy/Downloads/Wikipedia Data/bigrams/enwiki-20190301-pages-articles.bigrams_" + i + ".txt")

      // build count map
      var lineNum = 0
      val bigramCounts: mutable.HashMap[String, Int] = mutable.HashMap()
      for(bigram <- in.getLines()){
        lineNum += 1
        if(lineNum % 1000000 == 0) print(".")
        bigramCounts.put(bigram, bigramCounts.withDefault(_ => 0)(bigram) + 1)
      }

      // write bigram counts
      val out_file = new java.io.FileOutputStream("/home/troy/Downloads/Wikipedia Data/bigrams/enwiki-20190301-pages-articles.bigrams.counts" + i + ".txt")
      val bigram_stream = new java.io.PrintStream(out_file)
      bigramCounts.filter(_._2 > 1).foreach(entry => bigram_stream.println(entry._1 + " " + entry._2))
      bigram_stream.print(bigramCounts.values.sum)
      bigram_stream.close
    }
  }
}