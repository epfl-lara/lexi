package lexi

/**
  * Created by gs on 20.09.17.
  */
case class Token(id: Int, name: String) {
  def min(other: Token): Token =
    if (id <= other.id) this else other
}

object ErrorToken extends Token(Int.MaxValue, s"(error)")
