package wikiprocessor

import scala.collection.mutable
import scala.io.Source


abstract class GramModel {
  def getPercentage(token: String): Double
  def getTotal: BigInt
}


class SingletonGramModel(counts: Map[String,Int]) extends GramModel {
  val total: BigInt = counts.values.map(BigInt.apply).sum

  override def getPercentage(token: String): Double = {
    counts.withDefault(_ => 1)(token) / total.doubleValue()
  }

  override def getTotal: BigInt = {
    total
  }
}

class ShardedGramModel(allcounts: List[Map[String,Int]]) extends GramModel {
  val total: BigInt = allcounts.map(_.values.map(BigInt.apply).sum).sum

  override def getPercentage(token: String): Double = {
    val count = allcounts.map(_.withDefault(_ => 0)(token)).sum
    Math.max(count, 1) / total.doubleValue()
  }

  override def getTotal: BigInt = {
    total
  }
}

object GramModel {
  private def fileToMap(filename: String): Map[String,Int] = {
    println("Processing " + filename)
    val in = Source.fromFile(filename)
    val counts: mutable.HashMap[String, Int] = mutable.HashMap()
    var lineNum = 0
    for( count <- in.getLines()){
      count.split(" ", -1) match {
        case Array(args @ _*) if args.size > 1 => {
          lineNum += 1
          if(lineNum % 2000000 == 0) println("Lines loaded: " + lineNum)
          else if(lineNum % 100000 == 0) print(".")
          counts.put(args.dropRight(1).mkString(" "), args.takeRight(1)(0).toInt)
        }
        case _ =>
      }
    }
    println("\nProcessed total: " + lineNum + "\n")
    counts.toMap
  }

  def fromFile(filename: String): GramModel = {
    new SingletonGramModel(fileToMap(filename))
  }

  def fromFiles(filenames: List[String]): GramModel = {
    val maps = filenames.map(fileToMap)
    new ShardedGramModel(maps)
  }
}

