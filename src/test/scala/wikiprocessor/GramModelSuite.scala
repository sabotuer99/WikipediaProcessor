package wikiprocessor
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GramModelSuite extends FunSuite {

  test("load unigram model"){
    val unimodel = GramModel.fromFile("/home/troy/Downloads/Wikipedia Data/unigrams/enwiki-20190301-pages-articles.unigrams.counts.txt")

    val line = "the quick brown fox jumped over the lazy dog"
    line.split(" ").
      map((e: String) => (e.padTo(10, " ").mkString, unimodel.getPercentage(e))).
      distinct.
      sortBy(_._2).
      reverse.
      foreach(entry => println(entry._1 + ": " + "%1.9f".format(entry._2)))

    println(unimodel.getTotal)
  }

  test("load sharded bigram model"){
    val shards = (0 to 99).toList.map(
      "/home/troy/Downloads/Wikipedia Data/bigram_counts-second_attempt/" +
        "enwiki-20190301-pages-articles.bigrams.counts" + _ + ".txt"
      )
    val bimodel = GramModel.fromFiles(shards)

    val line = "the quick brown fox jumped over the lazy dog"
    val bigrams = line.split(" ").zip(line.split(" ").drop(1)).map(entry => entry._1 + " " + entry._2)
    bigrams.
      map((e: String) => (e.padTo(16, " ").mkString, bimodel.getPercentage(e))).
      distinct.
      sortBy(_._2).
      reverse.
      foreach(entry => println(entry._1 + ": " + "%1.9f".format(entry._2)))

    println(bimodel.getTotal)
  }


}
