package lexi

/**
  * Created by gs on 20.09.17.
  */
case class Token(priority: Int, name: String) {
  def min(other: Token): Token =
    if (priority <= other.priority) this else other

  override def toString(): String = s"<$name>"
}

object ErrorToken extends Token(Int.MaxValue, s"(error)")
