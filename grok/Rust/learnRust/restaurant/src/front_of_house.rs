pub mod hosting {
    pub fn add_to_waitlist(customer: &str) {
        let customer = String::from(customer);
        println!("Please wait here, {}.", customer);
    }

    pub fn seat_at_table() {
        println!("Have a seat.");
    }
}

pub mod serving {

    use crate::back_of_house::{Appetizer,Toast};

    pub fn take_order(toast: Toast, appetizer: Appetizer) {
        println!("May I take your order?");
        let mut meal = crate::back_of_house::Breakfast::summer_bf(toast, appetizer);
        // change mind if white bread
        if let Toast::White = meal.toast {
            println!("Customer changed their mind.");
            meal.toast = Toast::Wheat;
        }
        crate::back_of_house::submit_order(meal);
    }

    pub fn serve_order() {
        println!("Here you go, bon appetit!");
    }

    pub fn take_payment() {
        println!("Cash or credit?");
    }
}
