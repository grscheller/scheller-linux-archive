mod front_of_house;
mod back_of_house;

pub use crate::front_of_house::{hosting,serving};
pub use crate::back_of_house::{kitchen,special_services};
pub use crate::back_of_house::Appetizer::{Soup,Salad,NoApp};
pub use crate::back_of_house::Toast::{White,Wheat,Rye,SourDough};
pub use crate::back_of_house::Breakfast;

pub fn eat_at_restaurant() {
    println!();

    hosting::add_to_waitlist("Dr. Strange");
    hosting::seat_at_table();
    serving::take_order(Rye, Soup);
    serving::serve_order();
    special_services::fix_incorrect_order();
    serving::take_payment();
    
    println!();

    hosting::add_to_waitlist("Mr. Bogus");
    hosting::seat_at_table();
    serving::take_order(White, NoApp);
    serving::serve_order();
    serving::take_payment();
    special_services::call_bouncer();
    
    println!();

    hosting::add_to_waitlist("Miss Nami");
    hosting::seat_at_table();
    serving::take_order(SourDough, Salad);
    serving::serve_order();
    serving::take_payment();
}
