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
