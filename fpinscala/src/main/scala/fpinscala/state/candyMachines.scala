/** Program to model a candy dispensing machine 
 *  using fpinscala.state.State monad.
 *
 *    Doing as a standalone application as
 *    opposed to a library.  Using the
 *    CandyMachineTest object as a namespace.
 *
 *    Only reason for the package I am puttng it
 *    into is for grouping all the chapter 6
 *    programs together.
 *
 */
package fpinscala.chap06.candyMachines

import fpinscala.state.State

object CandyMachines{

  type Coins = Int
  type Candies = Int
  type CandyMachine = State[Machine, (Coins, Candies)]
  val  CandyMachine = State

  /** Inputs to a candy machine */
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  /** Locking states of a candy machine */
  sealed trait LockState
  case object Open extends LockState
  case object Locked extends LockState

  /** Machine is the State of a candy machine. */
  case class Machine(
                       locked: LockState
                    , candies: Candies
                    ,   coins: Coins    ) {

  //  def simulateMachine(inputs: List(Input)): CandyMachine =

  }

  /** Evolutionary approach to document my
   *  understanding (or lack thereof) of
   *  the State monad.
   */
  def main(args: Array[String]): Unit = {
    val initState = Machine(Locked, candies=100, coins=20)
    
    // This candy machine is broken, it will not take
    // coins and turning the handle doesn't do anything.
    val brokenCandyMachine =
      new CandyMachine(s => ((s.coins, s.candies), s))

    print("The broken candy machine initially has ")
    brokenCandyMachine(initState) match {
      case (coins: Coins, candies: Candies) => {
        println( candies + " Candies and " + coins + " Coins.")
      }
    }

  }
}
