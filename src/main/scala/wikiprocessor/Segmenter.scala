package wikiprocessor

import scala.util.hashing.MurmurHash3

class Segmenter(baseFile: String, segments: Int) {
  val streamMap = (for (i <- 0 until segments) yield (i, new java.io.PrintStream(new java.io.FileOutputStream(baseFile + "_" + i + ".txt")))).toMap

  def closeAll = streamMap.foreach(_._2.close())

  def write(line: String): Unit ={
    val index = Math.abs(MurmurHash3.stringHash(line)) % segments
    streamMap(index).println(line)
  }

}
