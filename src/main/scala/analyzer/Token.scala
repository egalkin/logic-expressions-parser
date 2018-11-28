package analyzer

object Token extends Enumeration {
  type Token = Value
  val LPAREN, RPAREN, END, IMPLICATION, NOT, AND, XOR, OR, ID, INVALID = Value
}
