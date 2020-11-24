# Rust programming language

Rust is a safe, hybrid functional/imperative programming language
good for both systems and general purpose computer programming.

A good introduction to the Rust, know as the
[book](https://doc.rust-lang.org/book)
can be found on the Rust
[web site](https://www.rust-lang.org).

## 1. C has its short comings

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

### 2. Locally installing Rust toolchain

The Rust toolchain really, really, really wants to be installed
locally into a developer's home directory.  Following the Rust website,

```
   $ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

The Rust tool chain will be installed here: `~/.cargo/bin`

To later update the installed toolchains,

```
   $ rustup update
```

### 3. First steps Rust toolchain - [baby\_steps](baby_steps/)

Create a new project with a main function,

```
   $ cargo new --bin baby_steps
        Created binary (application) baby_steps project
```

If I had not created it within a git repository, it would have set up the
baby\_steps directory it created as a git repo with .git file.  I put a copy
of the .gitignore file it would have created in the baby\_steps directory.

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
   $ cargo run
      Compiling baby_steps v0.1.2 (/home/grs/devel/scheller-linux-archive/grok/Rust/learnRust/baby_steps)
       Finished dev [unoptimized + debuginfo] target(s) in 0.22s
        Running `target/debug/baby_steps`
   Hello, world!
```

This also created a Cargo.lock file.  This file contains the configuration
of the last successful complilation of the project.  I will add this file
to the Git repo.

Executable can be run from the commandline,

```
   $ ./target/debug/baby_steps
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

The executable is `./target/release/baby_steps`.

### 4. Guessing game - [guessing\_game](guessing_game/)

Guessing game example taken from the official Rust
[book](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html).

I have added comments to the code documenting language features.

### 5. Fibonacci program - [fibonacci](fibonacci/)

Program to compute the nth Fiboncci number.

### 6. Twelve Days of Christmas - [twelve-days](fibonacci/)

Prints out the lyrics to the song "The Twelve Days of Christmas"

### 7. Restaurant - [restaurant](restaurant/)

Program to explore Rust's module system.

This program was the first where I really had to think about
Rust's ownership system.  It is not as simple as making all
calls by immutable reference.  Also, Rust complains when you
take a reference to something that is part of something else
which has a shared reference.
