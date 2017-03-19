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
  case object Open extends LockState
  case object Locked extends LockState

  /** Machine represents the State of a candy machine. */
  case class Machine(  locked: LockState
                    , candies: Candies
                    ,   coins: Coins    )

//  def simulation(inputs: List[Input]): State[Machine,(Coins, Candies)] =

  def main(args: Array[String]): Unit = {

    val initState = Machine(Locked, candies=100, coins=20)
    
  }
}
