package lexi

/**
  * Created by gs on 20.09.17.
  */
case class Token(id: Int, name: String) {
  def max(other: Token): Token =
    if (other.id > id) other else this
}
