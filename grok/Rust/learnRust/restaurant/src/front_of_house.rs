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
