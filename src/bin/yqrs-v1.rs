use std::{env, process};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("invalid argument");
        process::exit(1);
    }

    let query = yqrs::v1::parser::parse(&args[1]).unwrap();
    println!("{:#?}", query);
}
