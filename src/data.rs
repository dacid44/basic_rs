//! Holds types and functions for dealing with data operations.
//!
//! May be split into `data` and `expression` (or some other split) in the future.

use std::any::Any;
use std::fmt;
use std::fmt::{Formatter, write};
use std::ops::{Add, Div, Mul, Sub};
use std::str::FromStr;
use crate::State;
use crate::error::{bubble_result, ResultKind, DataTypeResult, TypeError, DataResultKind, NameError, ErrorKind, TypeResult, BasicError};

// macro_rules! data_type_num_op {
//     ($a:expr, $b:expr, $f:tt) => {
//         match ($a, $b) {
//             (DataType::Integer(a), DataType::Integer(b)) => Ok(DataType::Integer($f(a, b))),
//             (DataType::Float(a), DataType::Float(b)) => Ok(DataType::Float($f(a, b))),
//             _ => Err(()),
//         }
//     };
// }

macro_rules! matching_types {
    ($v:path, $f:expr) => {
        ($v(a), $v(b)) => Ok($v($f(a, b)))
    }
}

/// Used to hold a specifier for a type of data without the data itself.
#[derive(Debug)]
pub(crate) enum EmptyDataType {
    String,
    Integer,
    Float,
    /// A numerical type, currently `Integer` or `Float`.
    Number,
    Boolean,
}

// impl EmptyDataType {
//     pub(crate) fn fill(self, )
// }

/// Holds the available data types.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum DataType {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
}

impl DataType {
    /// Attempts to extract data from a [`DataType`]. May not be very useful, might be better to just
    /// use [`TryFrom`] directly.
    pub(crate) fn extract<T: TryFrom<Self, Error = TypeError>>(self) -> Result<T, TypeError> {
        self.try_into()
    }

    /// Return the [`EmptyDataType`] branch corresponding to the type of `self`.
    pub(crate) fn get_empty(&self) -> EmptyDataType {
        match self {
            Self::String(_) => EmptyDataType::String,
            Self::Integer(_) => EmptyDataType::Integer,
            Self::Float(_) => EmptyDataType::Float,
            Self::Boolean(_) => EmptyDataType::Boolean,
        }
    }

    /// Test if `self` is a numeric type.
    pub(crate) fn is_numeric(&self) -> bool {
        matches!(self, Self::Integer(_) | Self::Float(_))
    }

    /// Test if `self` matches the type specified by `test_type`.
    pub(crate) fn is_type(&self, test_type: &EmptyDataType) -> bool {
        match (self, test_type) {
            (Self::String(_), EmptyDataType::String) => true,
            (Self::Integer(_), EmptyDataType::Integer) => true,
            (Self::Float(_), EmptyDataType::Float) => true,
            (Self::Integer(_) | Self::Float(_), EmptyDataType::Number) => true,
            (Self::Boolean(_), EmptyDataType::Boolean) => true,
            _ => false,
        }
    }

    /// Similar to [`is_type()`](Self::is_type()), but returns an [`Option`] containing `self` if
    /// the type matches `test_type`.
    pub(crate) fn check_type(self, test_type: &EmptyDataType) -> Option<Self> {
        if self.is_type(test_type) { Some(self) } else { None }
    }

    /// Helper function to get a [`TypeError`].
    ///
    ///
    pub(crate) fn type_error<T>(self, context: &str, expected: EmptyDataType) -> TypeResult<T> {
        Err(TypeError {
            context: context.to_string(),
            expected,
            actual: self,
        })
    }

    fn pow(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a.pow(b as u32))),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a.powf(b))),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("Power (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("Power (invalid type)", self.get_empty()),
        }
    }

    fn typed_eq(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::String(a), Self::String(b)) => Ok(Self::Boolean(*a == b)),
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a == b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a == b)),
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a == b)),
            (_, b) => b.type_error("Equals (type mismatch)", self.get_empty()),
        }
    }

    fn typed_ne(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::String(a), Self::String(b)) => Ok(Self::Boolean(*a != b)),
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a != b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a != b)),
            (Self::Boolean(a), Self::Boolean(b)) => Ok(Self::Boolean(*a != b)),
            (_, b) => b.type_error("Equals (type mismatch)", self.get_empty()),
        }
    }

    fn typed_lt(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a < b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a < b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("LessThan (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("LessThan (invalid type)", self.get_empty()),
        }
    }

    fn typed_le(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a <= b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a <= b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("LessThanEquals (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("LessThanEquals (invalid type)", self.get_empty()),
        }
    }

    fn typed_gt(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a > b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a > b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("GreaterThan (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("GreaterThan (invalid type)", self.get_empty()),
        }
    }

    fn typed_ge(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Boolean(*a >= b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Boolean(*a >= b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("GreaterThanEquals (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("GreaterThanEquals (invalid type)", self.get_empty()),
        }
    }
}

impl TryFrom<DataType> for String {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::String(x) = value {
            Ok(x)
        } else {
            value.type_error("extract", EmptyDataType::String)
        }
    }
}

impl TryFrom<DataType> for i64 {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::Integer(x) = value {
            Ok(x)
        } else {
            value.type_error("Extract", EmptyDataType::Integer)
        }
    }
}

impl TryFrom<DataType> for f64 {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::Float(x) = value {
            Ok(x)
        } else {
            value.type_error("Extract", EmptyDataType::Float)
        }
    }
}

impl TryFrom<DataType> for bool {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::Boolean(x) = value {
            Ok(x)
        } else {
            value.type_error("Extract", EmptyDataType::Boolean)
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DataType::String(val) => write!(f, "{}", val),
            DataType::Integer(val) => write!(f, "{}", val),
            DataType::Float(val) => write!(f, "{}", val),
            DataType::Boolean(val) => write!(f, "{}", val),
        }
    }
}

impl Add for DataType {
    type Output = DataTypeResult;

    fn add(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::String(a), Self::String(b)) => Ok(Self::String(a.clone() + &b)),
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a + b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a + b)),
            (Self::String(_) | Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("Add (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("Add (invalid type)", self.get_empty()),
        }
    }
}

impl Sub for DataType {
    type Output = DataTypeResult;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a - b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a - b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("Subtract (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("Subtract (invalid type)", self.get_empty()),
        }
    }
}

impl Mul for DataType {
    type Output = DataTypeResult;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a * b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a * b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("Divide (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("Multiply (invalid type)", self.get_empty()),
        }
    }
}

impl Div for DataType {
    type Output = DataTypeResult;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a / b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a / b)),
            (Self::Integer(_) | Self::Float(_), b) =>
                b.type_error("Divide (type mismatch)", self.get_empty()),
            (_, b) => b.type_error("Divide (invalid type)", self.get_empty())
        }
    }
}

pub(crate) enum Expression {
    Variable(String),
    Literal(DataType),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Power(Box<Expression>, Box<Expression>),
    Equals(Box<Expression>, Box<Expression>),
    NotEquals(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessThanEquals(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanEquals(Box<Expression>, Box<Expression>),
}

impl Expression {
    pub(crate) fn eval(&self, state: &State) -> DataResultKind {
        match self {
            Expression::Variable(name) => state.vars.get(name)
                .map(|x| x.clone())
                .ok_or(ErrorKind::NameError(
                    NameError { context: "Eval (name not found)".to_string(), name: name.clone() })),
            Expression::Literal(data) => Ok(data.clone()),
            Expression::Add(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a + b, "Eval"),
            Expression::Subtract(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a - b, "Eval"),
            Expression::Multiply(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a * b, "Eval"),
            Expression::Divide(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a / b, "Eval"),
            Expression::Power(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.pow(b), "Eval"),
            Expression::Equals(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.typed_eq(b), "Eval"),
            Expression::NotEquals(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.typed_ne(b), "Eval"),
            Expression::LessThan(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.typed_lt(b), "Eval"),
            Expression::LessThanEquals(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.typed_le(b), "Eval"),
            Expression::GreaterThan(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.typed_gt(b), "Eval"),
            Expression::GreaterThanEquals(a, b) => combine(
                a.eval(state), b.eval(state), |a, b| a.typed_ge(b), "Eval"),
        }
    }
}

fn combine<T, F>(
    a: ResultKind<T>,
    b: ResultKind<T>,
    f: F,
    context: &str
) -> ResultKind<T>
    where
        F: FnOnce(T, T) -> TypeResult<T>
{
    match (a, b) {
        (Ok(a), Ok(b)) => bubble_result(f(a, b), context),
        (Err(err), _) | (_, Err(err)) => Err(err.bubble(context)),
    }
}
