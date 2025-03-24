fn main() {
    let lines = [
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
        "first", "second", "third", "forth", "fifth", "sixth", "seventh", "eighth",
        "ninth", "tenth", "eleventh", "twelfth",
    ];

    for (ii, day) in days.iter().enumerate() {
        println!("On the {} day of Christmas my true love sent to me", day);
        for line in lines.iter().skip(11 - ii) {
            println!(line)
        }
        println!()
    }
    println!("Merry Christmas,");
    println!("and a Happy New Year!");
}
