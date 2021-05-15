# Learn the Go Language

* Both an interpreter and compiler
* Simplified C like syntax

1. [Baby Steps](#baby-steps)

## Baby Steps

### hello

Traditional "Hello World" program
[hello.go](babysteps/hello.go).

Run as an interpreter:

```
   $ go run hello.go
   Hello, World!
```

Compile and then run:

```
   $ go build hello.go
   $ ./hello
   Hello, World!
```

### bomb

Countdown program to an explosion
[bomb.go](babysteps/bomb.go).

Illustrate the use of a "select" statement.  Similar in syntax to a
"switch" statement, but the cases refer to communication channels.
Select statements let goroutines wait on multiple communication channels.
