// Compute the nth Fibonacci number where
//   0 <= n <= 92
// and
//   fib(0) = 0
//   fib(1) = 1
//

use std::io;
use std::io::Write;

// Compute the nth Fibonacci number.
fn fib(n: u64) -> u64 {
    let mut n = n;
    let mut f_tup: (u64, u64) = (1, 0);
    while n > 0 {
        f_tup = (f_tup.1, f_tup.0 + f_tup.1);
        n = n - 1;
    }
    f_tup.1
}

fn main() {
    println!();
    if cfg!(debug_assertions) {
        eprintln!("[[ Running debug version of code! ]]");
    }
    println!("Compute the nth Fibonacci number.\n");

    let n: u64;

    // Get n from user.
    loop {
        let mut n_user = String::new();
        print!("Input n: ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut n_user)
            .expect("Failed to read user input");
        n = match n_user.trim().parse::<u64>() {
            Ok(num) => num,
            Err(_)  => continue
        };
        break;
    }

    // Evaluate fib(n) only if 64-bit integer overflow will not happen.
    if n < 93 {
        println!("fib({}) = {}", n, fib(n));
    } else {
        println!("n={} too large, fib({}) will overflow u64 integer", n, n)
    }

}
