import java.io.ByteArrayInputStream

import analyzer.{LexicalAnalyzer, Token}
import org.scalatest.{FlatSpec, Matchers}

class LexerTest extends FlatSpec with Matchers{
  "Lexer" should "return valid tokens from string" in {
    val testStream = new ByteArrayInputStream("a | b".getBytes)
    val lexer = new LexicalAnalyzer(testStream)
    lexer.nextToken()
    val correctSequence = Array(Token.ID, Token.OR, Token.ID, Token.END)
    for (token <- correctSequence) {
      lexer.curToken should be (token)
      lexer.nextToken()
    }
  }
}
