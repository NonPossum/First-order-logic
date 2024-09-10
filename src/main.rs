use std::collections::{HashMap, HashSet};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Term {
    Variable(String),
    Constant(String),
    Function(String, Vec<Term>),
}

impl Term {
    fn var<S: Into<String>>(name: S) -> Self {
        Term::Variable(name.into())
    }

    fn constant<S: Into<String>>(name: S) -> Self {
        Term::Constant(name.into())
    }

    fn function<S: Into<String>>(name: S, args: Vec<Term>) -> Self {
        Term::Function(name.into(), args)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Variable(name) => write!(f, "{}", name),
            Term::Constant(name) => write!(f, "{}", name),
            Term::Function(name, args) => {
                write!(f, "{}({})", name, args.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(", "))
            }
        }
    }
}

struct Model {
    domain: Domain,
    interpretation: Interpretation,
}

impl Model {
    fn new() -> Self {
        Model {
            domain: HashSet::new(),
            interpretation: HashMap::new(),
        }
    }

    fn add_to_domain<S: Into<String>>(&mut self, element: S) {
        self.domain.insert(element.into());
    }

    fn add_interpretation<S: Into<String>, F>(&mut self, predicate_name: S, func: F)
    where
        F: Fn(&[String]) -> bool + 'static,
    {
        self.interpretation.insert(predicate_name.into(), Box::new(func));
    }

    fn evaluate(&self, formula: &Formula) -> bool {
        formula.evaluate(&self.domain, &self.interpretation, &HashMap::new())
    }

    fn print_evaluation(&self, name: &str, formula: &Formula) {
        println!("{}: {}", name, formula);
        println!("Evaluation result: {}", self.evaluate(formula));
        println!();
    }
}

fn forall<S: Into<String>>(var: S, formula: Formula) -> Formula {
    Formula::for_all(var, formula)
}

fn exists<S: Into<String>>(var: S, formula: Formula) -> Formula {
    Formula::exists(var, formula)
}

fn implies(antecedent: Formula, consequent: Formula) -> Formula {
    Formula::implies(antecedent, consequent)
}

fn and(left: Formula, right: Formula) -> Formula {
    Formula::and(left, right)
}

fn or(left: Formula, right: Formula) -> Formula {
    Formula::or(left, right)
}

fn not(formula: Formula) -> Formula {
    Formula::not(formula)
}


#[derive(Debug, Clone)]
struct Predicate {
    name: String,
    terms: Vec<Term>,
}

impl Predicate {
    fn new<S: Into<String>>(name: S, terms: Vec<Term>) -> Self {
        Self {
            name: name.into(),
            terms,
        }
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.terms.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
    }
}

#[derive(Debug, Clone)]
enum Quantifier {
    ForAll(String),
    Exists(String),
}

#[derive(Debug, Clone)]
enum Formula {
    Predicate(Predicate),
    Not(Box<Formula>),
    And(Box<Formula>, Box<Formula>),
    Or(Box<Formula>, Box<Formula>),
    Implies(Box<Formula>, Box<Formula>),
    Quantified(Quantifier, Box<Formula>),
}

impl Formula {
    fn predicate<S: Into<String>>(name: S, terms: Vec<Term>) -> Self {
        Formula::Predicate(Predicate::new(name, terms))
    }

    fn not(formula: Formula) -> Self {
        Formula::Not(Box::new(formula))
    }

    fn and(left: Formula, right: Formula) -> Self {
        Formula::And(Box::new(left), Box::new(right))
    }

    fn or(left: Formula, right: Formula) -> Self {
        Formula::Or(Box::new(left), Box::new(right))
    }

    fn implies(antecedent: Formula, consequent: Formula) -> Self {
        Formula::Implies(Box::new(antecedent), Box::new(consequent))
    }

    fn for_all<S: Into<String>>(variable: S, formula: Formula) -> Self {
        Formula::Quantified(Quantifier::ForAll(variable.into()), Box::new(formula))
    }

    fn exists<S: Into<String>>(variable: S, formula: Formula) -> Self {
        Formula::Quantified(Quantifier::Exists(variable.into()), Box::new(formula))
    }
}

impl fmt::Display for Formula {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Formula::Predicate(p) => write!(f, "{}", p),
            Formula::Not(expr) => write!(f, "¬({})", expr),
            Formula::And(expr1, expr2) => write!(f, "({} ∧ {})", expr1, expr2),
            Formula::Or(expr1, expr2) => write!(f, "({} ∨ {})", expr1, expr2),
            Formula::Implies(expr1, expr2) => write!(f, "({} → {})", expr1, expr2),
            Formula::Quantified(q, expr) => match q {
                Quantifier::ForAll(v) => write!(f, "∀{}.{}", v, expr),
                Quantifier::Exists(v) => write!(f, "∃{}.{}", v, expr),
            },
        }
    }
}

type Domain = HashSet<String>;
type Interpretation = HashMap<String, Box<dyn Fn(&[String]) -> bool>>;

impl Formula {
    fn evaluate(&self, domain: &Domain, interpretation: &Interpretation, variable_assignment: &HashMap<String, String>) -> bool {
        match self {
            Formula::Predicate(p) => {
                let args: Vec<String> = p.terms.iter().map(|t| match t {
                    Term::Variable(v) => variable_assignment.get(v).unwrap_or(v).clone(),
                    Term::Constant(c) => c.clone(),
                    Term::Function(_, _) => panic!("Function evaluation not implemented"),
                }).collect();
                interpretation.get(&p.name).expect("Predicate not found in interpretation")(&args)
            },
            Formula::Not(expr) => !expr.evaluate(domain, interpretation, variable_assignment),
            Formula::And(expr1, expr2) => expr1.evaluate(domain, interpretation, variable_assignment) && expr2.evaluate(domain, interpretation, variable_assignment),
            Formula::Or(expr1, expr2) => expr1.evaluate(domain, interpretation, variable_assignment) || expr2.evaluate(domain, interpretation, variable_assignment),
            Formula::Implies(expr1, expr2) => !expr1.evaluate(domain, interpretation, variable_assignment) || expr2.evaluate(domain, interpretation, variable_assignment),
            Formula::Quantified(q, expr) => match q {
                Quantifier::ForAll(var) => domain.iter().all(|val| {
                    let mut new_assignment = variable_assignment.clone();
                    new_assignment.insert(var.clone(), val.clone());
                    expr.evaluate(domain, interpretation, &new_assignment)
                }),
                Quantifier::Exists(var) => domain.iter().any(|val| {
                    let mut new_assignment = variable_assignment.clone();
                    new_assignment.insert(var.clone(), val.clone());
                    expr.evaluate(domain, interpretation, &new_assignment)
                }),
            },
        }
    }
}

macro_rules! var {
    ($name:expr) => {
        Term::var($name)
    };
}

macro_rules! constant {
    ($name:expr) => {
        Term::constant($name)
    };
}

macro_rules! function {
    ($name:expr, $($arg:expr),*) => {
        Term::function($name, vec![$($arg),*])
    };
}

macro_rules! predicate {
    ($name:expr, $($term:expr),*) => {
        Formula::predicate($name, vec![$($term),*])
    };
}

fn main() {
    let mut model = Model::new();

    model.add_to_domain("KingOfFrance");

    model.add_interpretation("King", |args: &[String]| {
        args[0] == "KingOfFrance"
    });

    model.add_interpretation("Bald", |args: &[String]| {
        args[0] == "" // assume that the King of France is not bald
    });

    model.add_interpretation("Equals", |args: &[String]| {
        args[0] == args[1]
    });

    //∃x ((Kx ∧ ∀y (Ky → y = x)) ∧ Bx)
    let formula_king_bald = exists("x", and(
        and(
            predicate!("King", var!("x")),
            forall("y", implies(predicate!("King", var!("y")), predicate!("Equals", var!("y"), var!("x"))))
        ),
        predicate!("Bald", var!("x"))
    ));

    model.print_evaluation("The current King of France is bald", &formula_king_bald);
}
