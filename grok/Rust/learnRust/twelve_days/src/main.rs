fn main() {
    let lines = [
        "day of Christmas my true love sent to me",
        "Twelve drummers drumming,",
        "Eleven pipers piping,",
        "Ten lords a-leaping,",
        "Nine ladies dancing,",
        "Eight maids a-milking,",
        "Seven swans a-swimming,",
        "Six geese a-laying,",
        "Five golden rings...,",
        "Four calling birds,",
        "Three french hens,",
        "Two turtle doves, and",
        "A partridge in a pear tree.",
    ];

    let days = [
        "zeroth", "first", "second", "third", "forth", "fifth", "sixth", "seventh", "eighth",
        "ninth", "tenth", "eleventh", "twelfth",
    ];

    for ii in 1..13 {
        if ii != 1 {
            println!();
        }
        println!("On the {} {}", days[ii], lines[0]);
        for jj in (13 - ii)..13 {
            println!("{}", lines[jj]);
        }
    }
}
