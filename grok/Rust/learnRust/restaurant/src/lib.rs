// All this code lives under the implicit crate module

mod front_of_house {
    pub mod hosting {
        pub fn add_to_waitlist() {
            println!("Wait here noble customer.");
        }

        pub fn seat_at_table() {
            println!("Have a seat.");
        }
    }

    pub mod serving {
        pub fn take_order() {
            println!("May I take your order?");
            let mut meal = crate::back_of_house::Breakfast::summer("rye");
            // change mind
            meal.toast = String::from("wheat");
            crate::back_of_house::submit_order(&meal);
        }

        pub fn serve_order() {
            println!("Here you go, bon appetit!");
        }

        pub fn take_payment() {
            println!("Cash or credit?");
        }
    }
}

pub mod back_of_house {
    pub struct Breakfast {
        pub appetizer_option: Option<Appetizer>,
        pub toast: String,
        seasonal_fruit: String,
    }

    impl Breakfast {
        pub fn summer(toast: &str) -> Breakfast {
            Breakfast {
                appetizer_option: None,
                toast: String::from(toast),
                seasonal_fruit: String::from("peaches"),
            }
        }
    }

    pub enum Appetizer {
        Soup,
        Salad,
    }
    
    pub fn submit_order(order: &Breakfast) {
        println!("One {} toast with {} needed!", order.toast, order.seasonal_fruit);
        kitchen::cook_order();
    }

    pub mod kitchen {
        pub fn cook_order() {
            chop_food();
            fry_food();
        }

        fn chop_food() {
            println!("chop, chop,");
        }

        fn fry_food() {
            println!("sizzle, sizzle.");
        }
    }
    
    pub mod special_services {
        pub fn fix_incorrect_order() {
            super::kitchen::cook_order();
            crate::front_of_house::serving::serve_order();
        }

        pub fn call_bouncer() {
            println!("Buddy, you got dishes to wash.");
        }
    }
}

use crate::front_of_house::serving;
use crate::back_of_house::special_services;

pub fn eat_at_restaurant() {
    crate::front_of_house::hosting::add_to_waitlist();
    front_of_house::hosting::seat_at_table();
    serving::take_order();
    serving::serve_order();
    special_services::fix_incorrect_order();
    serving::take_payment();
    special_services::call_bouncer();
}
