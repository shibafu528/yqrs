#[derive(Debug)]
pub enum Expression {
    Cons(Cons),
    Atom(Atom),
}

impl Expression {
    pub fn is_nil(&self) -> bool {
        match self {
            Expression::Cons(c) => c.is_nil(),
            Expression::Atom(a) => a.is_nil(),
        }
    }
}

impl From<Cons> for Expression {
    fn from(c: Cons) -> Self {
        Self::Cons(c)
    }
}

impl From<Atom> for Expression {
    fn from(a: Atom) -> Self {
        Self::Atom(a)
    }
}

#[derive(Debug)]
pub struct Cons {
    car: Box<Expression>,
    cdr: Box<Expression>,
}

impl Cons {
    pub(crate) fn new(car: Box<Expression>, cdr: Box<Expression>) -> Cons {
        Cons { car, cdr }
    }

    pub(crate) fn empty() -> Cons {
        Cons {
            car: Box::new(Expression::Atom(Atom::Nil)),
            cdr: Box::new(Expression::Atom(Atom::Nil)),
        }
    }

    pub fn car(&self) -> &Expression {
        &self.car
    }

    pub fn cdr(&self) -> &Expression {
        &self.cdr
    }

    pub fn is_nil(&self) -> bool {
        self.car.is_nil() && self.cdr.is_nil()
    }
}

#[derive(Debug)]
pub enum Atom {
    Nil,
    Symbol(String),
    String(String),
    Integer(i64),
    Float(f64),
}

impl Atom {
    pub fn is_nil(&self) -> bool {
        match self {
            Atom::Nil => true,
            _ => false
        }
    }
}
