package lexi

/**
  * Created by gs on 20.09.17.
  */

class Lexer(dfa: DFA) {
  import Lexer._

//  def apply(input: Iterable[Char]): Stream[Token] = {
//
//  }

  private val NoState = new State(-1)

  def apply(input: String): Result = {
    var tokens: List[Token] = Nil
    var chunks: List[String] = Nil
    var consumedPos: Int = 0
    var failure: Option[Failure] = None

    var lastFinal: State = NoState
    var lastFinalPos: Int = -1
    var pos = 0
    var state = dfa.initState

    def finishChunk(): Unit = {
      assert(lastFinal != NoState, s"lastFinal should have been set due to the presence of a default token!")
      if (dfa.finalStates(lastFinal) == ErrorToken) {
        failure =
          Some(Failure(s"Illegal token '${input.substring(consumedPos, lastFinalPos)}' at position $consumedPos"))
      } else {
        tokens = dfa.finalStates(lastFinal) :: tokens
        chunks = input.substring(consumedPos, lastFinalPos) :: chunks
        consumedPos = lastFinalPos
      }
    }

    val deadState = dfa.findDeadState().getOrElse(null)
    while (failure.isEmpty && pos < input.length) {
      var c = input.charAt(pos)
      assert(dfa.alphabet.contains(c), s"Alphabet does not contain input character '$c'!")
      state = dfa.trans(state, c)

      if (state == deadState) {
        finishChunk()
        lastFinal    = NoState
        pos          = lastFinalPos
        lastFinalPos = -1
        state        = dfa.initState

      } else {
        pos += 1
        if (dfa.finalStates.contains(state)) {
          lastFinal    = state
          lastFinalPos = pos
        }
      }
    }

    if (failure.isEmpty)
      finishChunk()

    failure getOrElse Success(tokens.reverse, chunks.reverse)
  }
}

object Lexer {
  def apply(dfa: DFA): Lexer = new Lexer(dfa)

  sealed trait Result { val isSuccess: Boolean }
  case class Success(tokens: List[Token], chunks: List[String]) extends Result { val isSuccess = true }
  case class Failure(msg: String) extends Result { val isSuccess = false }
}


abstract class LexerDef {
  private var tokenDefs = Map.empty[Token, Regex]
  private var frozen = false

  implicit class TokenHelper(tokenName: String) {
    def :=(regex: Regex): Token = {
      assert(!frozen, s"LexerDef cannot be modified once it has been frozen")
      val token = lexi.Token(tokenDefs.size + 1, tokenName)
      tokenDefs = tokenDefs.updated(token, regex)
      token
    }
  }

  lazy val alphabet: Set[Sym] = {
    frozen = true
    tokenDefs.valuesIterator.toSet.flatMap((r: Regex) => r.alphabet)
  }

  lazy val toLexer: Lexer = {
    frozen = true
    val catchallRegex = Regex.Element(alphabet)
    val tokenDefs1 = tokenDefs + (ErrorToken -> catchallRegex)
    val dfa = DFA(NFA(tokenDefs1)).minimized()
    Lexer(dfa)
  }
}