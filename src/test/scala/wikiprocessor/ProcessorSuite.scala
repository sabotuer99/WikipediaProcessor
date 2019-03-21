package wikiprocessor

import scala.io.Source

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ProcessorSuite extends FunSuite {

  val autismPath = getClass.getResource("autism.txt").getPath

  test("clean autism article"){

    val in = Source.fromFile(autismPath)
    val raw = in.mkString

    val clean = Processor.clean(raw)

    assert(clean.startsWith("Autism is a developmental disorder"))
  }

}
