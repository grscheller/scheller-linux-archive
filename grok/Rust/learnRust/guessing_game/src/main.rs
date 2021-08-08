// The comments below are more about my early initial observations
// regarding the Rust language than this guessing game application.

// An external library ot "crate"
extern crate rand;

// Importing names into this namespace.
//
// Language likes terminating many things with semicolens, like these
// compile time nonexecutable statements.  An exception, though, is
// that a final return value in a scope does not get semicolen terminated.
// But, language does have a return statement.
//
use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    println!("\nGuess the number!\n");

    // Immutable by default, use let mut to make mutable.
    //
    // Like Python ranges, exclusive of 2nd value
    //
    let secret_number = rand::thread_rng().gen_range(1, 101);

    loop {
        // Without the "ln", does not flush to stdout before the read_line.
        println!("Please input your guess.");

        // new is an associated function to type String, similar
        // to a static method of a class in an OOP language.
        let mut guess = String::new();

        // Default indenting seems to always be 4 spaces, aligning the
        // "." seems to me more readable.
        //
        // First line returns an io::Result, basically an either made
        // to act like an option:
        //
        // Either an Ok, containing the number of bytes read, or is an Err
        //
        // The underlying std::result::Result, which the io::Result type
        // is based, is actually an enumeration.  Methods are added to
        // the enumeration via some sort of mechanism which uses the
        // impl keyword.  Enumerations seem to fill the roles of case
        // classes in Scala and ADTs in Haskell.
        //
        // Second line is a method on io::Result, If an Err, the expect
        // method will terminate the program and prints to stderr the
        // measage: Failed to read line: <whatever was in the Err>
        //
        io::stdin().read_line(&mut guess)
                   .expect("Failed to read line");

        // Note the reuse of guess as a different type, has a
        // Python runtime flavor to it.  Not declared mut???  I think
        // this is a totally different variable shadowing the guess
        // of type String above.
        //
        // Use of the "either-like" Result type saves the C practice of
        // constantly checking return values and global ERRORNUM variables.
        //
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_)  => continue
        };

        println!("You guessed: {}", guess);

        // Like a Haskell case statement or the Scala match operator.
        //
        // The "arms" are comma separated, no semicolons, dangling
        // commas on last arm OK.
        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big"),
            Ordering::Equal   => {
                println!("You win!");
                break
            }
        }
    }
}
