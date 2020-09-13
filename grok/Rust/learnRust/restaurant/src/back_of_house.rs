pub struct Breakfast {
    pub appetizer: Appetizer,
    pub toast: Toast,
    seasonal_fruit: String,
}

impl Breakfast {
    pub fn summer_bf(toast: Toast, app: Appetizer) -> Breakfast {
        Breakfast {
            appetizer: app,
            toast: toast,
            seasonal_fruit: String::from("peaches"),
        }
    }
}

pub enum Appetizer {
    Soup,
    Salad,
    NoApp,
}

pub enum Toast {
    White,
    Wheat,
    Rye,
    SourDough,
}

pub fn submit_order(order: Breakfast) {
    print!("One {} toast with {} ", toast_to_string(order.toast), order.seasonal_fruit);
    match order.appetizer {
        Appetizer::Soup  => println!("and a Soup are needed"),
        Appetizer::Salad => println!("and a Salad are needed"),
        Appetizer::NoApp => println!("is needed!"),
    };
    kitchen::cook_order();
}

fn toast_to_string(toast: Toast) -> String {
    match toast {
        Toast::White     => String::from("white"),
        Toast::Wheat     => String::from("wheat"),
        Toast::Rye       => String::from("rye"),
        Toast::SourDough => String::from("sour dough"),
    }
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
