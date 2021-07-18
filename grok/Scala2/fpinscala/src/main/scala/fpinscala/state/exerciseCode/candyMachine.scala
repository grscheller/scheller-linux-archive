/** Program to model a candy dispensing machine
  *  using fpinscala.state.State monad.
  *
  *     Doing as a standalone application as
  *     opposed to a library.  Using the
  *     Candy object as a namespace.
  *
  *  Rules:
  *  1. Inserting a coin into a locked machine will
  *     cause it to unlock if there’s any candy left.
  *  2. Turning the knob on an unlocked machine will
  *     cause it to dispense candy and become locked.
  *  3. Turning the knob on a locked machine or inserting
  *     a coin into an unlocked machine does nothing.
  *  4. A machine that’s out of candy ignores all inputs.
  */
package fpinscala.chap06.state

import fpinscala.state.State

object candyMachine {

  import State._

  type Coins = Int
  type Candies = Int

  /** Inputs to a candy machine */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  /** Locking states of a candy machine */
  sealed trait LockState
  case object Unlocked extends LockState
  case object Locked extends LockState

  /** Machine represents the state of a candy machine. */
  case class Machine(locked: LockState, candies: Candies, coins: Coins)

  /** Based on an Input, select next candy machine state. */
  def selectUpdate(i: Input)(m: Machine): Machine =
    (i, m) match {
      case (_, Machine(_, 0, _))           => m
      case (Turn, Machine(Locked, _, _))   => m
      case (Coin, Machine(Unlocked, _, _)) => m
      case (Turn, Machine(Unlocked, candy, coin)) =>
        Machine(Locked, candy - 1, coin)
      case (Coin, Machine(Locked, candy, coin)) =>
        Machine(Unlocked, candy, coin + 1)
    }

  def simulation(inputs: List[Input]): State[Machine, (Coins, Candies)] =
    for {
      _ <- sequence(inputs map { input => modify(selectUpdate(input)) })
      m <- get
    } yield (m.coins, m.candies)

  def main(args: Array[String]): Unit = {

    val initState = Machine(Locked, candies = 100, coins = 20)
    val inputs1 = List(Turn, Coin, Coin, Turn, Turn, Coin)
    val inputs2 = List(Turn, Coin, Coin, Turn, Turn, Coin, Turn)

    println()
    print("Initial State of candy machie is "); println(initState)

    val ((coin1, candy1), m1) = simulation(inputs1).run(initState)
    val ((coin2, candy2), m2) = simulation(inputs2).run(m1)

    print("\nFirst set inputs are "); println(inputs1.toString + ".")
    println(
      "After the first set of inputs, the candy machine has "
        + candy1 + " candies and " + coin1 + " coins."
    )
    println("The candy machine is now in state: " + m1)

    print("\nSecond set of inputs are "); println(inputs2.toString + ".")
    println(
      "After the second set of inputs, the candy machine has "
        + candy2 + " candies and " + coin2 + " coins."
    )
    println("The candy machine is now in state: " + m2)

    println()

  }
}
