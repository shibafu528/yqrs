use std::{env, process};
use yqrs::v1::eval::Context;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("invalid argument");
        process::exit(1);
    }

    let query = yqrs::v1::parser::parse(&args[1]).unwrap();
    println!("==> Parse result:\n{:#?}\n", query);

    let result = Context::new().evaluate(query.expression());
    println!("==> Eval result:\n{:#?}", result);
}
