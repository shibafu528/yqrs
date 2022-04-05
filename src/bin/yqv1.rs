use std::collections::HashMap;
use std::{env, process};
use yq::v1::eval::{Context, VariableProvider};
use yq::v1::expr::{Atom, Expression};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("invalid argument");
        process::exit(1);
    }

    let query = yq::v1::parser::parse(&args[1]).unwrap();
    println!("==> Parse result:\n{:#?}\n", query);

    let mut context = Context::new();
    let provider = Box::new(Provider {
        values: HashMap::from([(
            "?source",
            Expression::Atom(Atom::String("Yukari for Android".to_string())),
        )]),
    });
    context.set_variable_provider(provider);

    let result = context.evaluate(query.expression());
    println!("==> Eval result:\n{:#?}", result);
}

struct Provider {
    values: HashMap<&'static str, Expression>,
}

impl VariableProvider for Provider {
    fn get(&self, symbol: &str) -> Option<Expression> {
        self.values.get(symbol).map(Expression::clone)
    }
}
