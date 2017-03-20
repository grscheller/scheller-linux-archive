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
 *
 */
package fpinscala.chap06.state

import fpinscala.state.State

object candyMachine {

  type Coins = Int
  type Candies = Int

  /** Inputs to a candy machine */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  /** Locking states of a candy machine */
  sealed trait LockState
  case object Unlocked extends LockState
  case object   Locked extends LockState

  /** Machine represents the state of a candy machine. */
  case class Machine(  locked: LockState
                    , candies: Candies
                    ,   coins: Coins    )

  def update(i: Input)(m: Machine): ((Coins, Candies), Machine) = 
    (i, m) match {
      case (_, Machine(_, 0, _)) => ((m.coins, m.candies), m)
      case (Turn, Machine(  Locked, _, _)) => ((m.coins, m.candies), m)
      case (Coin, Machine(Unlocked, _, _)) => ((m.coins, m.candies), m)
      case (Turn, Machine(Unlocked, candy, coin)) => {
        val mm = Machine(Locked, candy-1, coin)
        ((mm.coins, mm.candies), mm)
      }
      case (Coin, Machine(Locked, candy, coin)) => {
        val mm = Machine(Unlocked, candy, coin+1)
        ((mm.coins, mm.candies), mm)
      }
    }

/* Work in progress - Maybe what I want is a list of machines, not (Coins, Candies) */
//  def simulation(inputs: List[Input]): State[Machine, List[(Coins, Candies)]] =
//    State.sequence(inputs.map(State.unit[Machine,(Coins, Candies)](State(update(_: Input): Machine => ((Coins, Candies), Machine)))))
//
//    State.sequence(inputs.map(State.unit[Machine,(Coins, Candies)](State(update(_)))))

  def main(args: Array[String]): Unit = {

    val initState = Machine(Locked, candies=100, coins=20)

    println("\nWork in progress.\n")
    
  }
}
