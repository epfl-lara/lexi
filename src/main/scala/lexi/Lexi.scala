package lexi

import Predef.{augmentString => _, wrapString => _, _}

object App {
  private def simpleLexerTest(): Unit =
  {
    object TestLexerDef extends LexerDef {
      import Regex._
      val IF   = "IF"   := "if"
      val ID   = "ID"   := (Alpha ~ AlphaDigit.*)
      val SPEC = "SPEC" := "-special"
      val WS   = "WS"   := C" ".+
    }

    val lexer = TestLexerDef.toLexer

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

  private def interact(): Unit =
  {
    object ArithLang extends LexerDef {
      import Regex._
      "IF"      := "if"
      "THEN"    := "then"
      "ELSE"    := "else"
      "ID"      := (Alpha ~ AlphaDigit.*)
      "NUM"     := Digit.+
      "LPAREN"  := "("
      "RPAREN"  := ")"
      "PLUS"    := "+"
      "MINUS"   := "-"
      "MUL"     := "*"
      "GT"      := ">"
      "EQ"      := "="
      "NOT"     := "!"
      "OR"      := "||"
      "AND"     := "&&"
      "WS"      := C" ".+
    }

    val lexer = ArithLang.toLexer

    var running = true
    println(s"Ready to lex.")
    do {
      import Predef.wrapString
      val line = scala.io.StdIn.readLine()
      if (line == null)
        running = false
      else
        lexer(line) match {
          case Lexer.Success(tokens, chunks) =>
            println(s"=>  ${tokens.mkString(", ")}")
          case Lexer.Failure(msg) =>
            println(s"!!  $msg")
        }

    } while (running)
  }

  def main(args: Array[String]): Unit = {
    simpleLexerTest()
    interact()
  }
}