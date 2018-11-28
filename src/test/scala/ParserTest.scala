import java.io.{BufferedWriter, ByteArrayInputStream, InputStream}
import java.text.ParseException
import java.util.Random

import analyzer.{Parser, Tree}
import generator.TestGenerator
import org.scalatest._



class ParserTest extends FlatSpec with Matchers {
  val r = new Random()

  "Parser" should "return Tree instance" in {
    val testStream = new ByteArrayInputStream("(!a | b) & a & (a | !(b ^ c))".getBytes)
    val parser = new Parser(testStream)
    parser.parse shouldBe a [Tree]
    for (i <- 0 to 10) {
      val testStream = new ByteArrayInputStream(TestGenerator.generateTest(r.nextInt(10)).getBytes)
      val parser = new Parser(testStream)
      parser.parse shouldBe a [Tree]
    }
  }

  it should "throw ParseException input contains unexpected symbols" in {
    var testStream = new ByteArrayInputStream("a#v".getBytes)
    var parser = new Parser(testStream)
    a [ParseException] should be thrownBy {
      parser.parse
    }
  }

  "Parser" should "return Tree instance in case of multiple negation" in {
    val testStream = new ByteArrayInputStream("!!!a".getBytes)
    val parser = new Parser(testStream)
    parser.parse shouldBe a [Tree]
  }

  it should "throw ParseException if input contains incorrect bracket sequence" in {
    var testStream = new ByteArrayInputStream("(a|b".getBytes)
    var parser = new Parser(testStream)
    a [ParseException] should be thrownBy {
      parser.parse
    }
    testStream = new ByteArrayInputStream("a|b)".getBytes)
    parser = new Parser(testStream)
    a [ParseException] should be thrownBy {
      parser.parse
    }
    testStream = new ByteArrayInputStream("((a)".getBytes)
    parser = new Parser(testStream)
    a [ParseException] should be thrownBy {
      parser.parse
    }
  }
}
