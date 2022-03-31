use crate::v1::expr::Expression;

#[derive(Debug)]
pub struct Query {
    sources: Vec<Source>,
    expression: Expression,
}

impl Query {
    pub(crate) fn new(sources: Vec<Source>, expression: Expression) -> Self {
        Query {
            sources,
            expression
        }
    }

    pub fn sources(&self) -> &[Source] {
        &self.sources
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

#[derive(Debug)]
pub struct Source {
    class: String,
    argument: Option<String>,
}

impl Source {
    pub(crate) fn new(class: String) -> Self {
        Source{
            class,
            argument: None,
        }
    }

    pub(crate) fn new_with_argument(class: String, argument: String) -> Self {
        Source {
            class,
            argument: Some(argument),
        }
    }

    pub fn class(&self) -> &str {
        &self.class
    }

    pub fn argument(&self) -> Option<&str> {
        match &self.argument {
            Some(s) => Some(s),
            None => None,
        }
    }
}
