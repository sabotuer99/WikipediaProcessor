package wikiprocessor
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GramModelSuite extends FunSuite {

  test("load unigram model"){
    val unimodel = GramModel.fromFile("/home/troy/Downloads/Wikipedia Data/unigrams/enwiki-20190301-pages-articles.unigrams.counts.txt")

    val line = "the quick brown fox jumped over the lazy dog"
    line.split(" ").foreach((e: String) => println(e.padTo(10, " ").mkString + ": " + "%1.9f".format(unimodel.getPercentage(e))))

    println(unimodel.getTotal)
  }

  test("load bigram model"){
    //GramModel.fromFile("/home/troy/Downloads/Wikipedia Data/bigram_counts-second_attempt/all_bigram_counts.txt"
  }


}
