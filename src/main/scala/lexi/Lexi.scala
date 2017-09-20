package lexi

import Predef.{augmentString => _, wrapString => _, _}

object App {
  private def process(tokenDefs: Map[Token, Regex]): DFA = {
    val nfa = NFA(tokenDefs)
    println(nfa)

    val dfa = DFA(nfa)
    println(dfa)

    val dfaMin = dfa.minimized()
    println(dfaMin)

    println(dfa.toGraphViz())
    println(dfaMin.toGraphViz())

    dfaMin
  }

  def main(args: Array[String]): Unit = {
    import Regex._
    process(Map(
      Token(1, "IF") -> "if",
      Token(2, "ID") -> (Alpha & AlphaDigit.*)
    ))
  }
}