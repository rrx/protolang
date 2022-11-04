use std::fmt;

pub fn format_list<T: fmt::Display>(args: &Vec<T>) -> String {
    args.iter()
        .map(|x| format!("{}", x))
        .collect::<Vec<_>>()
        .join(", ")
}

