# Learn the Go Language
* Both an interpreter and compiler
* Simplified C like syntax
1. [Baby Steps](#babysteps) 

## Baby Steps
### hello:
Traditional "Hello World" program in file `hello.go`
```
   package main
   
   import "fmt"
   
   func main() {
       fmt.Println("Hello, World!")
  ```
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

