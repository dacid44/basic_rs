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

#[derive(Debug)]
pub(crate) enum EmptyDataType {
    String,
    Integer,
    Float,
    Number,
}

#[derive(Clone, Debug)]
pub(crate) enum DataType {
    String(String),
    Integer(i64),
    Float(f64),
}

impl DataType {
    pub(crate) fn extract<T: TryFrom<Self, Error = TypeError>>(self) -> Result<T, TypeError> {
        self.try_into()
    }

    pub(crate) fn get_empty(&self) -> EmptyDataType {
        match self {
            Self::String(_) => EmptyDataType::String,
            Self::Integer(_) => EmptyDataType::Integer,
            Self::Float(_) => EmptyDataType::Float,
        }
    }

    fn pow(self, rhs: Self) -> DataTypeResult {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a.pow(b as u32))),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a.powf(b))),
            (Self::Integer(_) | Self::Float(_), b) => Err(TypeError {
                context: "Power (type mismatch)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
            (_, b) => Err(TypeError {
                context: "Power (invalid type)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
        }
    }
}

impl TryFrom<DataType> for String {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::String(x) = value {
            Ok(x)
        } else {
            Err(TypeError {
                context: "Extract".to_string(),
                expected: EmptyDataType::String,
                actual: value,
            })
        }
    }
}

impl TryFrom<DataType> for i64 {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::Integer(x) = value {
            Ok(x)
        } else {
            Err(TypeError {
                context: "Extract".to_string(),
                expected: EmptyDataType::Integer,
                actual: value
            })
        }
    }
}

impl TryFrom<DataType> for f64 {
    type Error = TypeError;

    fn try_from(value: DataType) -> Result<Self, Self::Error> {
        if let DataType::Float(x) = value {
            Ok(x)
        } else {
            Err(TypeError {
                context: "Extract".to_string(),
                expected: EmptyDataType::Float,
                actual: value,
            })
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DataType::String(val) => write!(f, "{}", val),
            DataType::Integer(val) => write!(f, "{}", val),
            DataType::Float(val) => write!(f, "{}", val),
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
            (Self::String(_) | Self::Integer(_) | Self::Float(_), b) => Err(TypeError {
                context: "Add (type mismatch)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
            (_, b) => Err(TypeError {
                context: "Add (invalid type)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
        }
    }
}

impl Sub for DataType {
    type Output = DataTypeResult;

    fn sub(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a - b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a - b)),
            (Self::Integer(_) | Self::Float(_), b) => Err(TypeError {
                context: "Subtract (type mismatch)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
            (_, b) => Err(TypeError {
                context: "Subtract (invalid type)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
        }
    }
}

impl Mul for DataType {
    type Output = DataTypeResult;

    fn mul(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a * b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a * b)),
            (Self::Integer(_) | Self::Float(_), b) => Err(TypeError {
                context: "Divide (type mismatch)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
            (_, b) => Err(TypeError {
                context: "Multiply (invalid type)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
        }
    }
}

impl Div for DataType {
    type Output = DataTypeResult;

    fn div(self, rhs: Self) -> Self::Output {
        match (&self, rhs) {
            (Self::Integer(a), Self::Integer(b)) => Ok(Self::Integer(a / b)),
            (Self::Float(a), Self::Float(b)) => Ok(Self::Float(a / b)),
            (Self::Integer(_) | Self::Float(_), b) => Err(TypeError {
                context: "Divide (type mismatch)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
            (_, b) => Err(TypeError {
                context: "Divide (invalid type)".to_string(),
                expected: self.get_empty(),
                actual: b,
            }),
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
}

impl Expression {
    pub(crate) fn eval(&self, state: &State) -> DataResultKind {
        match self {
            Expression::Variable(name) => state.vars.get(name)
                .map(|x| x.clone())
                .ok_or(ErrorKind::NameError(
                    NameError { context: "Eval (name not found)".to_string(), name: name.clone() })),
            Expression::Literal(data) => Ok(data.clone()),
            Expression::Add(a, b) => combine::<DataType, _>(
                a.eval(state), b.eval(state), |a, b| a + b, "Eval"),
            Expression::Subtract(a, b) => combine::<DataType, _>(
                a.eval(state), b.eval(state), |a, b| a - b, "Eval"),
            Expression::Multiply(a, b) => combine::<DataType, _>(
                a.eval(state), b.eval(state), |a, b| a * b, "Eval"),
            Expression::Divide(a, b) => combine::<DataType, _>(
                a.eval(state), b.eval(state), |a, b| a / b, "Eval"),
            Expression::Power(a, b) => combine::<DataType, _>(
                a.eval(state), b.eval(state), |a, b| a.pow(b), "Eval"),
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
