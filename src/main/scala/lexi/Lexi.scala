package lexi

import Predef.{augmentString => _, wrapString => _, _}

object App {
  private def compile(tokenDefs: Map[Token, Regex]): DFA = {
    val nfa    = NFA(tokenDefs)
    val dfa    = DFA(nfa)
    val dfaMin = dfa.minimized()
//    println(nfa)
//    println(dfa)
//    println(dfaMin)
//    println(dfaMin.toGraphViz())
    dfaMin
  }

  def main(args: Array[String]): Unit = {
    import Regex._

    val dfa = compile(Map(
      Token(1, "IF")   -> "if",
      Token(2, "ID")   -> (Alpha ~ AlphaDigit.*),
      Token(3, "SPEC") -> "-special",
      Token(4, "WS")   -> C" ".+,
      ErrorToken       -> (AlphaDigit | C" -")
    ))

//    println(dfa)
    val lexer = Lexer(dfa)

    assert(lexer("if i were there").isSuccess == true)
    assert(lexer("3a ").isSuccess             == false)
    assert(lexer("foo -special").isSuccess    == true)
    assert(lexer("foo -sp").isSuccess         == false)

    val ifIWereThere = lexer("if i were there")
    assert(ifIWereThere.isSuccess == true)

    {
      val Lexer.Success(tokens, chunks) = ifIWereThere
      assert(tokens.length == 7)
      assert(chunks.mkString("") == "if i were there")
    }
  }
}