use std::fmt;

use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{
    ser::{SerializeStruct, Serializer},
    Serialize,
};

pub fn format_list<T: fmt::Display>(args: &Vec<T>) -> String {
    args.iter()
        .map(|x| format!("{}", x))
        .collect::<Vec<_>>()
        .join(", ")
}
