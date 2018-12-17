## Rust systems programming language:
Explore the Rust toolchain and come up to speed with the language.  A good
introduction to the Rust, know as the
[book](https://doc.rust-lang.org/book) can be found on the Rust
[web site](https://www.rust-lang.org).

### 1. Why I no longer love C:
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
This program compiles without *even a warning message*, but when run
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
The problems are that booleans and ints are being conflated, assignment
returns the value being assigned, the < operator is left associative, and
the syntax is misleading.  See the paper,
[The Seven Deadly Sins of Introductory Programming Language Design](http://users.monash.edu/~damian/papers/PDF/SevenDeadlySins.pdf)
where I got this example.

Rust is a safer systems language than C.  Rust requires way too
much support from the underlying OS for it to be an alternative
to C as a "freestanding language" that can run on bare hardware.
In other words, it really is not suitable as a tool to write an
OS kernel.

### 2. Locally installing Rust toolchain:
The Rust toolchain really, really, really wants to be installed
locally into a developer's home directory.  To do so,
```
   $ curl -f https://sh.rushup.rs > rust.sh
   $ chmod u+x rust.sh
   $ ./rust.sh
```
The Rust tool chain will be installed here: `~/.cargo/bin`

I choose to make the "stable" toolchain the default.

To later update the installed toolchains,
```
   $ rustup update
```
### 3. First steps Rust toolchain - [baby_steps](baby_steps/):
Create a new project with a main function,
```
   $ cargo new --bin babySteps
        Created binary (application) `babySteps` project
```
If I had not created it within a git repository, it would have set up the
babySteps directory it created as a git repo with .git file.  I put a copy
of the .gitignore file it would have created in the babySteps directory.

The source code it created was the canonical "Hello World" program in the
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

This also created a Cargo.lock file.  This file contains the configuration
of the last successful complilation of the project.  I will add this file
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

### 4. Guessing game - [guessing_game](guessing_game/):
Guessing game example taken from the second edition of the official Rust 
[book](https://doc.rust-lang.org/book/second-edition/ch02-00-guessing-game-tutorial.html).

I have added comments to the code documenting language features.

### 5. Fibonacci program - [fibonacci](fibonacci/):
Program to compute the nth Fiboncci number.
