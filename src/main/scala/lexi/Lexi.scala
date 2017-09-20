package lexi

import Predef.{augmentString => _, wrapString => _, _}

object App {
  private def process(regex: Regex): DFA = {
    val nfa = NFA(regex)
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
    val r_aba = "ab".* & "a"
    process(r_aba)
  }
}