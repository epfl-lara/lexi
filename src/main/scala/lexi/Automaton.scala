package lexi

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Created by gs on 19.09.17.
  */

class State(val id: Int) extends AnyVal {
  override def toString(): String = s"($id)"
}


object Automaton {
  def checkValid[X](states: List[State], alphabet: Set[Sym],
                    trans: Map[(State, Sym), X],
                    initState: State, finalStates: List[State]): Unit = {
    require(states contains initState)
    require(finalStates.toSet subsetOf states.toSet)
    trans.keysIterator.foreach {
      case (_, EpsilonSym) =>
      case (_, c) => require(alphabet contains c)
    }
  }
}


case class DFA(states: List[State], alphabet: Set[Sym],
               trans: Map[(State, Sym), State],
               initState: State, finalStates: List[State])
{
  Automaton.checkValid(states, alphabet, trans, initState, finalStates)

//  digraph finite_state_machine {
//    rankdir=LR;
//    size="8,5"
//
//    node [shape = doublecircle]; S;
//    node [shape = point ]; qi
//
//    node [shape = circle];
//    qi -> S;
//    S  -> q1 [ label = "a" ];
//    S  -> S  [ label = "a" ];
//    q1 -> S  [ label = "a" ];
//    q1 -> q2 [ label = "b" ];
//    q2 -> q1 [ label = "b" ];
//    q2 -> q2 [ label = "b" ];
//  }

  def toGraphViz(): String = {
    def stateName(state: State) = s"S${state.id}"
    val nodeDecls = finalStates.map(s => s"node [shape = doublecircle]; ${stateName(s)}")
    val edgeDecls = trans.map { case ((s0, c), s1) => s"""${stateName(s0)} -> ${stateName(s1)} [ label = "$c" ]""" }
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

//  def withoutUnreachable(): DFA = {
//    @tailrec def closure(ss: Set[State]): Set[State] = {
//      val ss1 = alphabet.flatMap(c => ss.collect ... )
//      if (ss.size == ss1.size) ss else closure(ss1)
//    }
//    val newStates = closure(Set(initState))
//    DFA(newStates.toList, alphabet, trans.filter { case ((s0, _), _) => newStates.contains(s0) },
//      initState, finalStates.toSet.intersect(newStates).toList)
//  }

  def minimized(): DFA = {
    val finalStatesSet = finalStates.toSet
    val partition = mutable.ArrayBuffer[Set[State]](finalStatesSet, states.toSet diff finalStatesSet)

//    val invTrans = {
//      val acc = mutable.Map[(State, Sym), Set[State]]()
//      trans.foreach { case ((s0, c), s1) =>
//        val key = (s1, c)
//        acc.update(key, acc.getOrElse(key, Set.empty) + s0)
//      }
//      acc.toMap
//    }
    val invTrans = {
      for { ((s0, c), s1) <- trans.view }
        yield ((s1, c), s0)
    }.groupBy(_._1).mapValues(_.map(_._2).toSet)

    var worklist = mutable.ArrayBuffer[Set[State]](finalStates.toSet)
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

    println(partition)
    val oldStateMap = partition.view.flatMap(ss => ss.view.map(_ -> ss)).toMap
    val newStateMap = partition.view.map(_ -> freshState()).toMap
    val oldToNew = oldStateMap andThen newStateMap
    val transNew = {
      for {ss <- partition.view; c <- alphabet; oldS0 = ss.head }
        yield ((oldToNew(oldS0), c), oldToNew(trans(oldS0, c)))
    }.toMap
    DFA(newStateMap.values.toList, alphabet, transNew, oldToNew(initState), finalStatesSet.map(oldToNew).toList)
  }
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

    val states         = stateMap.values.toList
    val nfaFinalStates = nfa.finalStates.toSet
    val finalStates    = stateMap.collect { case (ss, state) if ss.exists(nfaFinalStates.contains) => state }
    DFA(states, nfa.alphabet, edges.toMap, stateMap(ssInit), finalStates.toList)
  }
}


case class NFA(states: List[State], alphabet: Set[Sym],
               trans: Map[(State, Sym), List[State]],
               initState: State, finalStates: List[State])
{
  Automaton.checkValid(states, alphabet, trans, initState, finalStates)
}

object NFA {
  def apply(regex: Regex): NFA = {
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

//    def transform(stateIn: State, regex: Regex)(ctx: (State, Boolean, Set[State]) => Set[State]): Set[State] = {
//      import Regex._
//      lazy val stateOut = freshState()
//      regex match {
//        case Epsilon =>
//          addEdge(stateIn, EpsilonSym, stateOut)
//          ctx(stateOut, true, Set(stateOut))
//
//        case Element(syms) =>
//          assert(!syms.contains(EpsilonSym))
//          syms.foreach(addEdge(stateIn, _, stateOut))
//          ctx(stateOut, false, Set(stateOut))
//
//        case Concat(r1, r2) =>
//          transform(stateIn, r1) { (out1, maybeEmpty1, finals1) =>
//            transform(out1, r2) { (out2, maybeEmpty2, finals2) =>
//              ctx(out2, maybeEmpty1 && maybeEmpty2, if (maybeEmpty2) finals1 union finals2 else finals2)
//            }
//          }
//
//        case Alt(r1, r2) =>
//          val in1 = freshState()
//          val in2 = freshState()
//          addEdge(stateIn, EpsilonSym, in1)
//          addEdge(stateIn, EpsilonSym, in2)
//          transform(in1, r1) { (out1, maybeEmpty1, finals1) =>
//            addEdge(out1, EpsilonSym, stateOut)
//            transform(in2, r2) { (out2, maybeEmpty2, finals2) =>
//              addEdge(out2, EpsilonSym, stateOut)
//              ctx(stateOut, maybeEmpty1 || maybeEmpty2, finals1 union finals2)
//            }
//          }
//
//        case Star(r) =>
//          addEdge(stateIn, EpsilonSym, stateOut)
//          transform(stateOut, r) { (outSub, maybeEmpty, finals) =>
//            addEdge(outSub, EpsilonSym, stateOut)
//            ctx(stateOut, true, Set(stateOut))  // CHECKME: ?! finals + stateOut ?!
//          }
//      }
//    }

    val initState = freshState()
    val finalStates = List[State](transform(initState, regex))
//    val finalStates = transform(initState, regex){ (_, _, fs) => fs }.toList
    NFA(states, alphabet - EpsilonSym, edges.toMap, initState, finalStates)
  }
}