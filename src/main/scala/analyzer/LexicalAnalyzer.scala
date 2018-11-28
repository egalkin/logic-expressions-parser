package analyzer

import java.io.InputStream

import Token.Token

class LexicalAnalyzer(val is: InputStream) {
  var curChar: Int = ' '
  var curPos: Int = 0
  var curToken: Token = Token.END
  nextChar()

  def isBlank(c: Int): Boolean = c == ' ' || c == '\r' || c == '\n' || c == '\t'

  def nextChar(): Unit = {
    curPos += 1
    curChar = is.read()
  }

  def nextToken(): Unit = {
    while (isBlank(curChar))
      nextChar()
    curChar match {
      case '(' => nextChar(); curToken = Token.LPAREN
      case ')' => nextChar(); curToken = Token.RPAREN
      case '|' => nextChar(); curToken = Token.OR
      case '&' => nextChar(); curToken = Token.AND
      case '^' => nextChar(); curToken = Token.XOR
      case '!' => nextChar(); curToken = Token.NOT
      case '-' =>
        nextChar()
        curChar match {
          case '>' => nextChar(); curToken = Token.IMPLICATION
          case _ => curToken = Token.INVALID
        }
      case c if c >= 65 && c <= 122 => nextChar(); curToken = Token.ID
      case -1 => curToken = Token.END
      case _ => curToken = Token.INVALID
    }
  }

}
