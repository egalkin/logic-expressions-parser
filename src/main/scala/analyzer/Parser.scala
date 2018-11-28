package analyzer

import java.io.{FileInputStream, InputStream}
import java.text.ParseException

import analyzer.Token.Token


object Parser extends App {
  val parser = new Parser(new FileInputStream("input.in"))
  Tree.treeToDotCode(parser.parse)
}

class Parser(val is: InputStream) {
  val lex: LexicalAnalyzer = new LexicalAnalyzer(is)
  val baseFirst = Set(Token.LPAREN, Token.ID, Token.NOT)
  val andBaseFirst = Set(Token.LPAREN, Token.ID)
  val andNotFirst = Set(Token.NOT)
  val expsFollow = Set(Token.RPAREN, Token.END)
  val implFollow = expsFollow + Token.IMPLICATION
  val orsFollow = implFollow + Token.OR
  val xorsFollow = orsFollow + Token.XOR


  lex.nextToken()

  def parse: Tree = {
    Exp match {
      case tree if lex.curToken == Token.END => tree
      case _ => throw generateEndOfInputParseException
    }
  }


  private def Exp: Tree = {
    lex.curToken match {
      case token if baseFirst.contains(token) =>
        val sub = Impl
        val cont = ExpPrime
        Tree("Exp", sub::cont::Nil)
      case _ => throw generateBaseParseException
    }
  }

  private def ExpPrime:Tree = {
    lex.curToken match {
      case Token.IMPLICATION =>
        lex.nextToken()
        val sub = Impl
        val cont = ExpPrime
        Tree("Exp'", Tree("->") :: sub :: cont :: Nil)
      case token if expsFollow.contains(token) => setToEmpty("Exp'")
      case _ => throw generateBaseParseException
    }
  }

  private def Impl: Tree = {
    lex.curToken match {
      case token if baseFirst.contains(token) =>
        val sub = Or
        val cont = ImplPrime
        Tree("Impl", sub :: cont :: Nil)
      case _ => throw generateBaseParseException
    }
  }

  private def ImplPrime: Tree = {
    lex.curToken match {
      case Token.OR =>
        lex.nextToken()
        val sub = Or
        val cont = ImplPrime
        Tree("Impl'", Tree("|") :: sub :: cont :: Nil)
      case token if implFollow.contains(token) => setToEmpty("Exp'")
      case _ => throw generateBaseParseException
    }
  }

  private def Or: Tree = {
    lex.curToken match {
      case token if baseFirst.contains(token) =>
        val sub = Xor
        val cont = OrPrime
        Tree("Or", sub :: cont :: Nil)
      case _ => throw generateBaseParseException
    }
  }

  private def OrPrime: Tree = {
    lex.curToken match {
      case Token.XOR =>
        lex.nextToken()
        val sub = Xor
        val cont = OrPrime
        Tree("Or'", Tree("^") :: sub :: cont :: Nil)
      case token if orsFollow.contains(token) => setToEmpty("Or'")
      case _ => throw generateBaseParseException
    }
  }


  private def Xor: Tree = {
    lex.curToken match {
      case token if baseFirst.contains(token) =>
        val sub = And
        val cont = XorPrime
        Tree("Xor", sub :: cont :: Nil)
      case _ => throw generateBaseParseException
    }
  }

  private def XorPrime: Tree = {
    lex.curToken match {
      case Token.AND =>
        lex.nextToken()
        val sub = And
        val cont = XorPrime
        Tree("Xor'", Tree("&") :: sub :: cont :: Nil)
      case token if xorsFollow.contains(token) => setToEmpty("Xor'")
      case _ => throw generateBaseParseException
    }

  }


  private def And: Tree = {
    lex.curToken match {
      case token if andBaseFirst.contains(token) =>
        val sub = Not
        Tree("And", sub :: Nil)
      case Token.NOT =>
        lex.nextToken()
        val sub = And
        Tree("And", Tree("!") :: sub :: Nil)
      case _ => throw generateBaseParseException
    }
  }

  private def Not: Tree = {
    lex.curToken match {
      case Token.ID =>
        lex.nextToken()
        Tree("Not", Tree("id") :: Nil)
      case Token.LPAREN =>
        lex.nextToken()
        val sub = Exp
        if (lex.curToken != Token.RPAREN)
          throw generateBracketParseException
        lex.nextToken()
        Tree("Not", Tree("(") :: sub :: Tree(")") :: Nil)
      case _ => throw generateBaseParseException
    }
  }

  private def setToEmpty(s: String) = Tree(s, Tree("e") :: Nil)

  private def generateEndOfInputParseException = new ParseException("Expect end of input, found: " + lex.curToken, lex.curPos)

  private def generateBracketParseException = new ParseException("Expect closing parenthesis, found: " + lex.curToken, lex.curPos)

  private def generateBaseParseException = new ParseException("Unexpected symbol " + lex.curChar.toChar + " at position: " + lex.curPos, lex.curPos)

}


