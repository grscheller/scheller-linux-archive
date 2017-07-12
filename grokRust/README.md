# Coming up to speed with the Rust systems programming language:

Explore the Rust toolchain and come up to speed with the language.

## 1. Why I no longer love C:
Consider code a beginner C programmer might write,
```
    #include <stdio.h>
    
    int main(void) {
        int x = 5;
        int y = 0;
        printf("%d\n", x);
        if (x=0 || -10 < y < 10) {
            printf("%d\n", x);
        } 
        return 0;
    }
```
This program compiles without *even a warning message*, but when run<br>
surprises the programmer with the output,
```
   5
   1
```
What the programmer probably meant on line 7 was,
```
    if ( x == 0 || (-10 < y) && (y < 10) ) {
```
and not,
```
    if ( x = (0 || ((-10 < y) < 10)) ) {
```
The problems are that booleans and ints are being conflated, assignment<br>
returns the value being assigned, the < operator is left associative, and<br>
the syntax is misleading.  See the paper, [The Seven Deadly
Sins](http://users.monash.edu/~damian/papers/PDF/SevenDeadlySins.pdf)
where I got this example.<br>

I think Rust will become the C for the 21st Century.

## 2. First steps Rust toolchain - [baby steps](babySteps/):
Create a new project with a main function,
```
   $ cargo new --bin babySteps
        Created binary (application) `babySteps` project
```
If I had not created it within a git repository, it would have set up the<br>
babySteps directory it created as a git repo with .git file.  I put a copy<br>
of the .gitignore file it would have created in the babySteps directory.

The source code it created was the canonical "Hello World" program in the<br>
file `src/main.rs`,
```
   fn main() {
       println!("Hello, world!");
   }
```
To build and run the code,
```
   $ cargo run
      Compiling babySteps v0.1.0 (file:///home/geoff/devel/scheller-linux-archive/grokRust/babySteps)
   warning: crate `babySteps` should have a snake case name such as `baby_steps`
     |
     = note: #[warn(non_snake_case)] on by default

       Finished dev [unoptimized + debuginfo] target(s) in 3.59 secs
        Running `target/debug/babySteps`
   Hello, world!
```
Since I never intend to publish babyStrps, I will keep the name as is.

This also created a Cargo.lock file.  This file contains the configuration<br>
of the last successful complilation of the project.  I will add this file<br>
to the Git repo.

Executable can be run from the commandline,
```
   $ ./target/debug/babySteps 
   Hello, world!
```
To just build,
```
   $ cargo build
```
Both the run and build targets build a "debug" version.

To build or run a "release" version with full optimization,
```
   $ cargo build --release
   $ cargo run --release
```
The executable is `./target/release/babySteps`.

## 3. Guessing game - [guessing_game](guessing_game/):
Guessing game example taken from the official Rust 
[book](https://doc.rust-lang.org/book/second-edition/ch02-00-guessing-game-tutorial.html).

I have added comments to the code documenting language features.
