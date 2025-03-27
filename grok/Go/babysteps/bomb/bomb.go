package main

import (
	"fmt"
	"time"
)

func main() {
	/*
	   "select" statements choose which set of send or receive
	   operations will proceed.  The Go Language random selects
	   amoung the communications channels which are ready to run
	*/

	tick := time.Tick(100 * time.Millisecond)
	boom := time.After(700 * time.Millisecond)

	for {
		// Not to be confused with a "switch" statement
		//   "switch" statements choose values in a definite order
		select {
		case <-tick:
			fmt.Println("tick.")
		case <-boom:
			fmt.Println("BOOM!")
			return
		default:
			fmt.Println("    .")
			time.Sleep(45 * time.Millisecond)
		}
	}
}
