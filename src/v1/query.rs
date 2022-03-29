pub struct Query {
    sources: Vec<Source>,
}

impl Query {
    pub(crate) fn new(sources: Vec<Source>) -> Query {
        Query {
            sources,
        }
    }

    pub fn sources(&self) -> &[Source] {
        &self.sources
    }
}

pub struct Source {
    class: String,
    argument: Option<String>,
}

impl Source {
    pub(crate) fn new(class: String) -> Source {
        Source{
            class,
            argument: None,
        }
    }

    pub(crate) fn new_with_argument(class: String, argument: String) -> Source {
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