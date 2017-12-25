package aoc2017

import scala.annotation.tailrec

import scala.io.Source

object Day25 {
  type Operation = (TuringMachine) => (TuringMachine)

  case class TuringMachine(state: String, checksumSteps: Int, transitions: Map[String, Operation] = Map(), position: Int = 0, tape: Set[Int] = Set()) {
    def currentValue = tape.contains(position)

    def next(value: Boolean, move: Int, state: String) = copy(state = state, position = position + move, tape = (if (value) tape + position else tape - position))

    def step = {
      val t = transitions(state)
      t(this)
    }

    def run(steps: Int = checksumSteps) = {
      @tailrec
      def run(steps: Int, current: TuringMachine): TuringMachine =
        if (steps == 0)
          current
        else
          run(steps - 1, current.step)

      run(steps, this)
    }

    def checksum = tape.size
  }
  object TuringMachine {
    val headRe = """(?s)Begin in state ([A-Z]).
Perform a diagnostic checksum after (\d+) steps.\s+(.*)""".r

    val stateRe = """(?s)In state ([A-Z]):\s*
  If the current value is (0|1):
    - Write the value (0|1).
    - Move one slot to the ((?:right)|(?:left)).
    - Continue with state ([A-Z]).
  If the current value is (0|1):
    - Write the value (0|1).
    - Move one slot to the ((?:right)|(?:left)).
    - Continue with state ([A-Z]).\s*(.*)""".r

    def apply(str: String) = {
      def parseValue(str: String) = {
        val v = str.toInt
        if (v == 1)
          true
        else if (v == 0)
          false
        else
          throw new MatchError(s"invalid value $str")
      }

      def parseMove(str: String) =
        if (str == "right")
          1
        else if (str == "left")
          -1
        else
          throw new MatchError(s"invalid move $str")

      @tailrec
      def parse(str: String = str, current: Option[TuringMachine] = None): Option[TuringMachine] =
        str match {
          case headRe(startState, checksum, remainder) => parse(remainder, Some(new TuringMachine(startState, checksum.toInt)))
          case stateRe(startState, currentValue1Str, writeValue1Str, move1Str, endState1, currentValue2Str, writeValue2Str, move2Str, endState2, remainder) => {
            val machine = current.get

            val newMachine = machine.copy(transitions = machine.transitions + {
                val currentValue1 = parseValue(currentValue1Str)
                val currentValue2 = parseValue(currentValue2Str)
                val writeValue1 = parseValue(writeValue1Str)
                val writeValue2 = parseValue(writeValue2Str)
                val move1 = parseMove(move1Str)
                val move2 = parseMove(move2Str)

                (startState -> {
                  (machine: TuringMachine) => {
                    if (machine.currentValue == currentValue1) {
                      machine.next(writeValue1, move1, endState1)
                    } else if (machine.currentValue == currentValue2) {
                      machine.next(writeValue2, move2, endState2)
                    } else
                        throw new MatchError("invalid transaction")
                  }
                })
              }
            )

            parse(remainder, Some(newMachine))
          }
          case "" => current
        }
      parse()
    }
  }

  def main(args: Array[String]) {
    val test = """Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."""

    val input = Source.fromFile("data/day25/input.txt").getLines.mkString("\n")

    val testMachine = TuringMachine(test).get
    val inputMachine = TuringMachine(input).get

    println("part 1 test: " + testMachine.run().checksum)
    println("part 2 input: " + inputMachine.run().checksum)
  }
}
