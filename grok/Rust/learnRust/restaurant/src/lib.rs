// All this code lives under the implicit crate module

mod front_of_house;
mod back_of_house;

pub use crate::front_of_house::{hosting,serving};
pub use crate::back_of_house::{kitchen,special_services};

pub fn eat_at_restaurant() {
    hosting::add_to_waitlist();
    hosting::seat_at_table();
    serving::take_order();
    serving::serve_order();
    special_services::fix_incorrect_order();
    serving::take_payment();
    special_services::call_bouncer();
}
