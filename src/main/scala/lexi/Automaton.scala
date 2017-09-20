package lexi

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by gs on 19.09.17.
  */

class State(val id: Int) extends AnyVal {
  override def toString(): String = s"($id)"
}

sealed abstract class Automaton[S] {
  val states: List[State]
  val alphabet: Set[Sym]
  val trans: Map[(State, Sym), S]
  val initState: State
  val finalStates: Map[State, Token]

  def checkValid(): Unit = {
    require(states contains initState)
    require(finalStates.keySet subsetOf states.toSet)
    trans.keysIterator.foreach {
      case (_, EpsilonSym) =>
      case (_, c) => require(alphabet contains c)
    }
  }
  checkValid()

  def edges(): Iterable[(State, Sym, State)]

  def toGraphViz(): String = {
    def stateName(state: State) = s"S${state.id}"
    val nodeDecls = states.map { s =>
      val name  = stateName(s)
      val label = s""", label="$name""""
      finalStates.get(s) match {
        case None =>
          s"""node [shape = circle$label ]; $name"""
        case Some(token) =>
          s"""node [shape = doublecircle$label, xlabel="${token.name}" ]; $name"""
      }
    }

    val edgeDecls = edges.map { case (s0, c, s1) => s"""${stateName(s0)} -> ${stateName(s1)} [ label = "$c" ]""" }
    s"""
       |digraph fsm {
       |  rankdir=LR;
       |
       |  node [shape = point]; in
       |  ${nodeDecls.mkString("\n  ")}
       |
       |  node [shape = circle];
       |  in -> ${stateName(initState)};
       |  ${edgeDecls.mkString("\n  ")}
       |}
     """.stripMargin
  }
}


case class DFA(states: List[State], alphabet: Set[Sym],
               trans: Map[(State, Sym), State],
               initState: State, finalStates: Map[State, Token]) extends Automaton[State]
{
  def edges() = trans.view.map { case ((s0, c), s1) => (s0, c, s1) }

  def minimized(): DFA = {
    val finalStatesSet = finalStates.keySet
    val tokenStatesMap: Map[Token, Set[State]] =
      finalStates.toList.groupBy(_._2).mapValues(_.map(_._1).toSet)

    val partition = mutable.ArrayBuffer[Set[State]]()
    partition.appendAll(tokenStatesMap.valuesIterator)
    partition.append(states.toSet diff finalStatesSet)

    val invTrans: Map[(State, Sym), Set[State]] = {
      for { ((s0, c), s1) <- trans.view }
        yield ((s1, c), s0)
    }.groupBy(_._1).mapValues(_.map(_._2).toSet)

    var worklist = mutable.ArrayBuffer[Set[State]]()
    worklist.appendAll(tokenStatesMap.valuesIterator)
    def pop(): Set[State] = {
      val n = worklist.length - 1
      val s = worklist.apply(n)
      worklist.reduceToSize(n)
      s
    }

    @inline def split(buf: mutable.ArrayBuffer[Set[State]], idx: Int, ec1: Set[State], ec2: Set[State]): Unit = {
      buf(idx) = ec1
      buf += ec2
    }

    while (worklist.nonEmpty) {
      val ecA = pop()

      for (c <- alphabet) {
        val ecX = ecA.flatMap(s => invTrans.getOrElse((s, c), Set.empty))

        for ((ecY, idxYp) <- partition.zipWithIndex) {
          val ecY1 = ecX intersect ecY
          lazy val ecY2 = ecY diff ecX

          if (ecY1.nonEmpty && ecY2.nonEmpty) {
            split(partition, idxYp, ecY1, ecY2)

            val idxYw = worklist.indexOf(ecY)
            if (idxYw >= 0)                   split(worklist, idxYw, ecY1, ecY2)
            else if (ecY1.size <= ecY2.size)  worklist += ecY1
            else                              worklist += ecY2
          }
        }
      }
    }

    var nextStateId: Int = 0
    def freshState(): State = {
      val state = new State(nextStateId)
      nextStateId += 1
      state
    }

    val oldStateMap: Map[State, Set[State]] = partition.view.flatMap(ss => ss.view.map(_ -> ss)).toMap
    val newStateMap: Map[Set[State], State] = partition.view.map(_ -> freshState()).toMap
    val oldToNew: State => State            = oldStateMap andThen newStateMap

    val transNew: Map[(State, Sym), State]  = {
      for {ss <- partition.view; c <- alphabet; oldS0 = ss.head }
        yield ((oldToNew(oldS0), c), oldToNew(trans(oldS0, c)))
    }.toMap

    val finalStatesNew: Map[State, Token]   = {
      val set = finalStatesSet.map(s => (oldToNew(s), finalStates(s)))
      val map = set.toMap
      assert(set.size == map.size)  // During minimization no distinctions between tokens were lost
      map
    }

    DFA(newStateMap.values.toList, alphabet, transNew, oldToNew(initState), finalStatesNew)
  }

  /** Returns the first state for which every transition loops back to itself.
    * (We only generate DFAs with at most one such state.)
    */
  def findDeadState(): Option[State] =
    states.find { s0 => alphabet.forall( c => trans(s0, c) == s0 ) }
}

object DFA {
  def apply(nfa: NFA): DFA = {
    @tailrec def epsClosure(ss: Set[State]): Set[State] = {
      val ss1 = ss union ss.flatMap(s => nfa.trans.getOrElse((s, EpsilonSym), List.empty))
      if (ss.size == ss1.size) ss else epsClosure(ss1)
    }

    def dfaTrans(ss: Set[State], c: Sym): Set[State] =
      epsClosure(ss.flatMap(s => nfa.trans.getOrElse((s, c), List.empty)))

    var stateMap = Map[Set[State], State]()
    var edges    = mutable.Map[(State, Sym), State]()
    val worklist = mutable.Queue[(State, Set[State])]()

    var nextStateId: Int = 0
    def freshState(ss: Set[State]): State = {
      val state = new State(nextStateId)
      nextStateId += 1
      stateMap = stateMap.updated(ss, state)
      worklist.enqueue((state, ss))
      state
    }

    val ssSink = Set.empty[State]
    val ssInit = epsClosure(Set(nfa.initState))
    Seq(ssSink, ssInit).foreach(freshState)

    while (worklist.nonEmpty) {
      val (state1, ss1) = worklist.dequeue()
      for (c <- nfa.alphabet) {
        val ss2 = dfaTrans(ss1, c)
        val state2 = stateMap.getOrElse(ss2, freshState(ss2))
        edges = edges.updated((state1, c), state2)
      }
    }

    // Remove sink state if it's unreachable
    val sSink = stateMap(ssSink)
    if (!edges.exists { case ((s0, c), s1) => s0 != sSink && s1 == sSink }) {
      stateMap = stateMap - ssSink
      edges = edges.filter { case ((s0, _), _) => s0 != sSink }
    }

    val states = stateMap.valuesIterator.toList
    val finalStates = {
      def findMaxToken(ss: Set[State]): Option[Token] =
        ss.foldLeft(None: Option[Token]){
          case (None, s)     => nfa.finalStates.get(s)
          case (Some(t1), s) => nfa.finalStates.get(s).map(t2 => t1 min t2).orElse(Some(t1))
        }
      for { (ss, state) <- stateMap; maxToken <- findMaxToken(ss) }
        yield (state, maxToken)
    }

    DFA(states, nfa.alphabet, edges.toMap, stateMap(ssInit), finalStates)
  }
}


case class NFA(states: List[State], alphabet: Set[Sym],
               trans: Map[(State, Sym), List[State]],
               initState: State, finalStates: Map[State, Token]) extends Automaton[List[State]]
{
  def edges() = trans.view.flatMap { case ((s0, c), ss1) => ss1.view.map(s1 => (s0, c, s1)) }
}

object NFA {
  def apply(tokenDefs: Map[Token, Regex]): NFA = {
    var states = List[State]()
    var alphabet = Set[Sym]()

    var nextStateId: Int = 0
    def freshState(): State = {
      val state = new State(nextStateId)
      nextStateId += 1
      states = state :: states
      state
    }

    var edges = mutable.Map[(State, Sym), List[State]]()
    def addEdge(s0: State, c: Sym, s1: State): Unit = {
      val t = (s0, c)
      alphabet = alphabet + c
      edges.update(t, s1 :: edges.getOrElse(t, List()))
    }

    def transform(stateIn: State, regex: Regex): State = {
      import Regex._
      lazy val stateOut = freshState()
      regex match {
        case Epsilon =>
          addEdge(stateIn, EpsilonSym, stateOut)
          stateOut

        case Element(syms) =>
          assert(!syms.contains(EpsilonSym))
          syms.foreach(addEdge(stateIn, _, stateOut))
          stateOut

        case Concat(r1, r2) =>
          val out1 = transform(stateIn, r1)
          transform(out1, r2)

        case Alt(r1, r2) =>
          val in1 = freshState()
          val in2 = freshState()
          addEdge(stateIn, EpsilonSym, in1)
          addEdge(stateIn, EpsilonSym, in2)
          addEdge(transform(in1, r1), EpsilonSym, stateOut)
          addEdge(transform(in2, r2), EpsilonSym, stateOut)
          stateOut

        case Star(r) =>
          addEdge(stateIn, EpsilonSym, stateOut)
          addEdge(transform(stateOut, r), EpsilonSym, stateOut)
          stateOut
      }
    }

    val initState = freshState()
    val finalStates = tokenDefs.map { case (token, regex) => transform(initState, regex) -> token }
    NFA(states, alphabet - EpsilonSym, edges.toMap, initState, finalStates)
  }
}