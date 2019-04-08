package wikiprocessor

import scala.collection.mutable
import scala.io.Source

class GramModel(counts: Map[String,Int]) {
  val total: BigInt = counts.values.map(BigInt.apply).sum

  def getPercentage(token: String): Double = {
    counts(token) / total.doubleValue()
  }

  def getTotal: BigInt = {
    total
  }
}

object GramModel {
  def fromFile(filename: String): GramModel ={
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
    new GramModel(counts.toMap)
  }
}

