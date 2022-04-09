use clap::Parser;
use std::collections::HashMap;
use std::process;
use yq::v1::eval::{Context, VariableProvider};
use yq::v1::expr::{Atom, Expression};

#[derive(Parser, Debug)]
#[clap(author, version)]
struct Args {
    /// Input query string
    query: String,

    /// Provide variable (format must be 'key=value')
    #[clap(short, long)]
    value: Vec<String>,
}

fn main() {
    let args = Args::parse();
    let mut variables = HashMap::new();
    for v in &args.value {
        match v.split_once('=') {
            Some((k, v)) => variables.insert(k.to_string(), Expression::Atom(Atom::String(v.to_string()))),
            _ => {
                eprintln!("--value format must be 'key=value'");
                process::exit(1);
            }
        };
    }

    let query = yq::v1::parser::parse(&args.query).unwrap();
    println!("==> Parse result:\n{:#?}\n", query);

    let mut context = Context::new();
    let provider = Box::new(Provider { values: variables });
    context.set_variable_provider(provider);

    let result = context.evaluate(query.expression());
    println!("==> Eval result:\n{:#?}", result);
}

struct Provider {
    values: HashMap<String, Expression>,
}

impl VariableProvider for Provider {
    fn get(&self, symbol: &str) -> Option<Expression> {
        self.values.get(symbol).map(Expression::clone)
    }
}
