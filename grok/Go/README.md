# Learn the Go Language

- Both an interpreter and compiler
- Simplified C like syntax

1. [Baby Steps](#baby-steps)
1. [First Modules](#first-modules)

## Baby Steps

Now using gopls as the LSP with Neovim after sitting on this project for
a while.

- Go likes to format with tabs, not spaces
- does not like `func main()` defined in two files in same directory
  - moved hello.go -> hello/
  - moved bomb.go -> bomb/
- both hello.go and bomb.go have `package main` declared as first line
  - basically I have two modules each with its own main entry point
- see [Go module layout](https://go.dev/doc/modules/layout) on `go.dev`
  - a module is made up of packages
  - executables have a package called main
    - package main can have multiple files
    - only one of which contains a function called main

### hello

Traditional "Hello World" program
[hello.go](babysteps/hello/hello.go).

Run as an interpreter:

```fish
    $ go run hello.go
    Hello, World!
```

Compile and then run:

```fish
    $ go build hello.go
    $ ./hello
    Hello, World!
```

### bomb

Countdown program to an explosion
[bomb.go](babysteps/bomb/bomb.go).

Illustrate the use of a "select" statement. Similar in syntax to a
"switch" statement, but the cases refer to communication channels.
Select statements let goroutines wait on multiple communication channels.

## First Modules

First Go modules, will be deployed on GitHub. I will follow the
[Create a Go module](https://go.dev/doc/tutorial/create-module) tutorial.

Will consist of a library and executable, both trivial.
