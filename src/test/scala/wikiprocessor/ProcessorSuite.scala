package wikiprocessor

import scala.io.Source

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ProcessorSuite extends FunSuite {

  val autismPath = getClass.getResource("autism.txt").getPath
  val anarchyPath = getClass.getResource("anarchy.txt").getPath
  val albedoPath = getClass.getResource("albedo.txt").getPath
  val neilsenPath = getClass.getResource("leslie_neilsen.txt").getPath

  test("clean autism article"){
    val in = Source.fromFile(autismPath)
    val raw = in.mkString
    val clean = Processor.clean(raw)
    assert(clean.startsWith("Autism is a developmental disorder"))
    assert(!clean.contains(" ref "))

    println(clean)
  }

  test("clean anarchy article"){
    val in = Source.fromFile(anarchyPath)
    val raw = in.mkString
    val clean = Processor.clean(raw)
    assert(clean.startsWith("Anarchism is an anti-authoritarian political philosophy"))
    assert(!clean.contains(" ref "))

    println(clean)
  }

  test("clean albedo article"){
    val in = Source.fromFile(albedoPath)
    val raw = in.mkString
    val clean = Processor.clean(raw)
    assert(clean.startsWith("Albedo meaning whiteness is the measure of"))
    assert(!clean.contains(" ref "))

    println(clean)
  }

  test("clean leslie_neilsen article"){
    val in = Source.fromFile(neilsenPath)
    val raw = in.mkString
    val clean = Processor.clean(raw)
    assert(clean.startsWith("Leslie William Nielsen February November was a Canadian actor"))
    assert(!clean.contains("wikitable"))

    println(clean)
  }

  test("interior quote ok"){
    val clean = Processor.clean("it's")
    assert(clean == "it's")
    println(clean)
  }

}
