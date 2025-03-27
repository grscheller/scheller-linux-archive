package main

import (
	"fmt"
	"time"
)

func main() {
	/* The `select` statement chooses which set of send or receive
	   operations will proceed. The Go Language random selects among the
	   communications channels which are ready to run.

	   Not to be confused with the "switch" statement.
	*/
	tick := time.Tick(100 * time.Millisecond)
	boom := time.After(700 * time.Millisecond)

	for {
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
