// Original: https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=ece81580f40d9cb8146928a3ef911e08
// thanks @kb10uy!
#[allow(unused_imports)]
use crate::v1::expr::{Atom, Cons, Expression};

pub trait Value {
    fn to_yq(&self) -> Expression;
}

impl Value for str {
    fn to_yq(&self) -> Expression {
        Expression::Atom(Atom::String(self.to_string()))
    }
}

impl Value for i64 {
    fn to_yq(&self) -> Expression {
        Expression::Atom(Atom::Integer(*self))
    }
}

impl Value for f64 {
    fn to_yq(&self) -> Expression {
        Expression::Atom(Atom::Float(*self))
    }
}

macro_rules! yq {
    // Symbol atom
    // foobar
    ($id:ident) => {
        Expression::Atom(Atom::Symbol(stringify!($id).to_string()))
    };

    // Integer atom
    // 12345
    ($l:literal) => {
        $l.to_yq()
    };

    // Reference atom
    // {12345}
    ( { $l:literal } ) => {
        Expression::Atom(Atom::Reference($l))
    };

    // Nil / empty list
    // ()
    ( ( ) ) => {
        Expression::Atom(Atom::Nil),
    };

    // List
    // (a b c 1 2 3)
    ( ( $car:tt $($cdr:tt)* ) ) => {
        // To elide ambiguousness, we put {} as a sentinel.
        Expression::Cons(
            Cons::new(
                Box::new(yq!($car)),
                Box::new(yq![$($cdr)* {}]),
            )
        )
    };

    // Internal: Nil
    [{}] => {
        Expression::Atom(Atom::Nil)
    };

    // Internal: List
    [$car:tt $($cdr:tt)*] => {
        Expression::Cons(
            Cons::new(
                Box::new(yq!($car)),
                Box::new(yq![$($cdr)*]),
            )
        )
    };
}

pub(crate) use yq;
