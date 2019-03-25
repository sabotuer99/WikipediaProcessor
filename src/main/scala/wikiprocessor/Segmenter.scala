package wikiprocessor

import java.io.PrintStream

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

class Segmenter(baseFile: String, segments: Int) {
  val streamMap: Map[Int, PrintStream] = (for (i <- 0 until segments) yield
    (i, new java.io.PrintStream(new java.io.FileOutputStream(baseFile + "_" + i + ".txt")))).toMap
  var buffer: mutable.ListBuffer[(Int, String)] = mutable.ListBuffer()

  def closeAll(): Unit = {
    writeBuffer()
    streamMap.foreach(_._2.close())
  }

  def write(line: String) = {
    val index = Math.abs(MurmurHash3.stringHash(line)) % segments
    buffer.append((index, line))

    if(buffer.size > 300000) writeBuffer()
  }

  def writeBuffer(){
    buffer groupBy (_._1) foreach(entry => {
      streamMap(entry._1).println(entry._2.map(_._2) mkString "\n")
      streamMap(entry._1).flush()
    })
    print("*")
    buffer.clear()
  }

}
