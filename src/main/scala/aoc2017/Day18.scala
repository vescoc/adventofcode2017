package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day18 {
  val sndRe = """snd\s+([a-z])""".r
  val setIntRe = """set\s+([a-z])\s+([-0-9]+)""".r
  val setRe = """set\s+([a-z])\s+([a-z])""".r
  val addIntRe = """add\s+([a-z])\s+([-0-9]+)""".r
  val addRe = """add\s+([a-z])\s+([a-z])""".r
  val mulIntRe = """mul\s+([a-z])\s+([-0-9]+)""".r
  val mulRe = """mul\s+([a-z])\s+([a-z])""".r
  val modIntRe = """mod\s+([a-z])\s+([-0-9]+)""".r
  val modRe = """mod\s+([a-z])\s+([a-z])""".r
  val rcvRe = """rcv\s+([a-z])""".r
  val jgzIntRe = """jgz\s+([a-z])\s+([-0-9]+)""".r
  val jgzRe = """jgz\s+([a-z])\s+([a-z])""".r
  val jgzIntIntRe = """jgz\s+([-0-9]+)\s+([-0-9]+)""".r

  object CPUState extends Enumeration {
    type CPUState = Value

    val running, waiting, stopped, deadlock = Value
  }

  import CPUState.CPUState

  trait Queue {
    def snd(v: Long)
    def rcv: Option[Long]
  }

  case class State(queue: Queue, cpuState: CPUState = CPUState.running, pc: Int = 0, regs: Map[String, Long] = Map()) {
    def snd(v: Long) = {
      queue.snd(v)
      copy(pc = pc + 1)
    }
    def rcv = queue.rcv
    def get(r: String) = regs.getOrElse(r, 0L)
    def +(p: (String, Long)) = copy(regs = regs + p, pc = pc + 1)
    def nop = copy(pc = pc + 1)
    def pc(off: Int) = copy(pc = pc + off)

    def waiting = copy(cpuState = CPUState.waiting)
    def running = copy(cpuState = CPUState.running)
    def stopping = copy(cpuState = CPUState.stopped)
    def deadlocking = copy(cpuState = CPUState.deadlock)

    def isStopped = cpuState == CPUState.stopped
    def isRunning = cpuState == CPUState.running
    def isDeadlocked = cpuState == CPUState.deadlock
    def isWaiting = cpuState == CPUState.waiting
  }

  def parse(line: String, rcvNopWith0: Boolean): (State) => (State, String) = {
    line match {
      case sndRe(r)  => (state: State) => {
        val rval = state.get(r)
        (state.snd(rval), s"snd $r=$rval")
      }
      case setIntRe(r1, v) => (state: State) => {
        (state + (r1 -> v.toInt), s"set $r1 $v")
      }
      case setRe(r1, r2) => (state: State) => {
        val r2val = state.get(r2)
        (state + (r1 -> r2val), s"set $r1 $r2=$r2val")
      }
      case addIntRe(r1, v) => (state: State) => {
        val r1val = state.get(r1)
        (state + (r1 -> (r1val + v.toInt)), s"add $r1=$r1val $v")
      }
      case addRe(r1, r2) => (state: State) => {
        val r1val = state.get(r1)
        val r2val = state.get(r2)
        (state + (r1 -> (r1val + r2val)), s"add $r1=$r1val $r2=$r2val")
      }
      case mulIntRe(r1, v) => (state: State) => {
        val r1val = state.get(r1)
        (state + (r1 -> (r1val * v.toInt)), s"mul $r1=$r1val $v")
      }
      case mulRe(r1, r2) => (state: State) => {
        val r1val = state.get(r1)
        val r2val = state.get(r2)
        (state + (r1 -> (r1val * r2val)), s"mul $r1=$r1val $r2=$r2val")
      }
      case modIntRe(r1, v) => (state: State) => {
        val r1val = state.get(r1)
        (state + (r1 -> (r1val % v.toInt)), s"mod $r1=$r1val $v")
      }
      case modRe(r1, r2) => (state: State) => {
        val r1val = state.get(r1)
        val r2val = state.get(r2)
        (state + (r1 -> (r1val % r2val)), s"mod $r1=$r1val $r2=$r2val")
      }
      case rcvRe(r) => if (rcvNopWith0)
        (state: State) => {
          val rval = state.get(r)
          if (rval != 0) {
            val v = state.rcv
            v match {
              case Some(value) => (state + (r -> value), s"rcv $r=$rval <- $value")
              case None => (state.waiting, s"rcv $r=$rval <- waiting")
            }
          } else
            (state.nop, s"rcv $r=$rval")
        }
        else
          (state: State) => {
            val rval = state.get(r)
            val v = state.rcv
            v match {
              case Some(value) => (state + (r -> value), s"rcv $r=$rval <- $value")
              case None => (state.waiting, s"rcv $r=$rval <- waiting")
            }
          }
      case jgzIntRe(r, v) => (state: State) => {
        val rval = state.get(r)
        ((if (rval <= 0) state.nop else state.pc(v.toInt)), s"jgz $r=$rval $v")
      }
      case jgzRe(r1, r2) => (state: State) => {
        val r1val = state.get(r1)
        val r2val = state.get(r2)
        ((if (r1val <= 0) state.nop else state.pc(r2val.toInt)), s"jgz $r1=$r1val $r2=$r2val")
      }
      case jgzIntIntRe(v1, v2) => (s: State) => {
        ((if (v1.toInt <= 0) s.nop else s.pc(v2.toInt)), s"jgz $v1 $v2")
      }
    }
  }

  @tailrec
  def execute(name: String, program: List[(State) => (State, String)], state: State, step: Int = 0, limit: Int = Integer.MAX_VALUE): (Int, State) = {
    if (state.isStopped || state.isDeadlocked)
      (step, state)
    else {
      val runningState = (
        if (state.isRunning)
          state
        else
          state.running
      )

      if (step == limit)
        (step, runningState)
      else {
        val pc = runningState.pc
        if (pc < 0 || pc >= program.size)
          (step, state.stopping)
        else {
          val (newState, debug) = program(pc)(state)
          //println(debug + ": " + state + " -> " + newState)
          //println(s"$name: $debug")
          if (newState.isRunning)
            execute(name, program, newState, step + 1, limit)
          else
            (step + 1, newState)
        }
      }
    }
  }

  class SimpleQueue extends Queue {
    private var q: Option[Long] = None

    override def rcv = None

    override def snd(v: Long) {
      q = Some(v)
    }

    override def toString = "SimpleQueue" + q.mkString("[", ",", "]")
  }

  class Msg {
    private var a: List[Long] = List()
    private var aSndCount = 0
    private var aRcvCount = 0

    private var b: List[Long] = List()
    private var bSndCount = 0
    private var bRcvCount = 0

    val queueA = new Queue {
      def snd(v: Long) {
        b = b ++ List(v)
        aSndCount = aSndCount + 1
      }

      def rcv = a match {
        case head :: tail => {
          a = tail
          aRcvCount = aRcvCount + 1
          Some(head)
        }
        case Nil => None
      }

      override def toString = s"(A snd=$aSndCount,rcv=$aRcvCount,size=${a.size}/${b.size})"
    }

    val queueB = new Queue {
      def snd(v: Long) {
        a = a ++ List(v)
        bSndCount = bSndCount + 1
      }

      def rcv = b match {
        case head :: tail => {
          b = tail
          bRcvCount = bRcvCount + 1
          Some(head)
        }
        case Nil => None
      }

      override def toString = s"(B snd=$bSndCount,rcv=$bRcvCount,size=${b.size}/${a.size})"
    }
  }
  
  def main(args: Array[String]) {
    val test = List("set a 1", "add a 2", "mul a a", "mod a 5", "snd a", "set a 0", "rcv a", "jgz a -1", "set a 1", "jgz a -2").map(line => parse(line, true))
    val inputNop = Source.fromFile("data/day18/input.txt").getLines.map(line => parse(line, true)).toList
    val input = Source.fromFile("data/day18/input.txt").getLines.map(line => parse(line, false)).toList

    {
      {
        val simpleQueue = new SimpleQueue()

        println(execute("main", test, State(simpleQueue), 0, 100))
      }

      {
        val simpleQueue = new SimpleQueue()

        println(execute("main", inputNop, State(simpleQueue), 0, 2000))
      }
    }

    {
      val msg = new Msg()

      @tailrec
      def ex(step1: Int = 0, state1: State = State(queue = msg.queueA, regs = Map("p" -> 0)), step2: Int = 0, state2: State = State(queue = msg.queueB, regs = Map("p" -> 1))): ((Int, State), (Int, State)) = {
        val (c1, s1) = execute("A", input, state1, limit = 4000)
        val (c2, s2) = execute("B", input, state2, limit = 4000)
        if (state1 == s1 && state2 == s2 && s1.isWaiting && s2.isWaiting)
          ((step1 + c1, s1.deadlocking), (step2 + c2, s2.deadlocking))
        else
          ex(step1 + c1, s1, step2 + c2, s2)
      }

      println(ex())
    }
  }
}
