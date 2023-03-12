package examples

import spire.util.Opt
import spire.syntax.cfor._

case class Bracket(tpe: Char, position: Int):
  def matchClosing(c: Char): Boolean =
    if (this.tpe == '[' && c == ']') return true
    if (this.tpe == '{' && c == '}') return true
    if (this.tpe == '(' && c == ')') return true
    false

type Stack = scala.collection.immutable.List[Bracket]

object CheckBrackets {

  object Opening:
    def unapply(c: Char): Option[Char] =
      if (c == '(' || c == '[' || c == '{') Some(c) else None

  object Closing:
    def unapply(c: Char): Option[Char] =
      if (c == ')' || c == ']' || c == '}') Some(c) else None

  def isBalanced(text: String): Int = {
    var stack: Stack = Nil
    for (position <- 0 until text.length) {
      val ch = text.charAt(position)
      ch match {
        case Opening(ch) =>
          // Process opening bracket
          stack = Bracket(ch, position) :: stack
        case Closing(ch) =>
          // Process closing bracket
          if (stack.isEmpty) return position
          if (stack.head.matchClosing(ch)) {
            // balanced
            stack = stack.tail
          } else {
            // not balanced
            return position
          }
        case _ => // skip other symbols
      }
    }
    if (stack.isEmpty) -1 else stack.head.position
  }
}

object CheckBracketsOpt {

  object Opening:
    def unapply(c: Char): Opt[Char] =
      if (c == '(' || c == '[' || c == '{') Opt(c) else Opt.empty

  object Closing:
    def unapply(c: Char): Opt[Char] =
      if (c == ')' || c == ']' || c == '}') Opt(c) else Opt.empty

  def isBalanced(text: String): Int = {
    var stack: Stack = Nil
    cforRange(0 until text.length) { position =>
      val ch = text.charAt(position)
      ch match {
        case Opening(ch) =>
          // Process opening bracket
          stack = Bracket(ch, position) :: stack
        case Closing(ch) =>
          // Process closing bracket
          if (stack.isEmpty) return position
          if (stack.head.matchClosing(ch)) {
            // balanced
            stack = stack.tail
          } else {
            // not balanced
            return position
          }
        case _ => // skip other symbols
      }
    }
    if (stack.isEmpty) -1 else stack.head.position
  }
}
