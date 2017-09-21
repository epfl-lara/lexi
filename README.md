# lexi

Lexi is a toy Scala implementation of a lexer generator.
It provides a small DSL that allows you to define tokens using regular expressions and consequently compiles them to a finite state automaton.
To produce the sequence of tokens from an input string, Lexi will then interpret said automaton.

See [Lexi.scala](https://github.com/epfl-lara/lexi/blob/86cf3fed4b040c94d2347a5628b10ad538c13195/src/main/scala/lexi/Lexi.scala#L35) for an example of how to use lexi to tokenize a simple input language.
