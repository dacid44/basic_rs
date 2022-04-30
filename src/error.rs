use std::any::Any;
use std::fmt::{Display, Formatter};
use std::io;
use std::io::Error;
use crate::data::EmptyDataType;
use crate::DataType;

pub(crate) trait BasicError: Sized + Display + Into<ErrorKind> {
    fn bubble(self, context: &str) -> ErrorKind {
        ErrorKind::Bubble {
            context: context.to_string(),
            contained: Box::new(self.into()),
        }
    }
}

pub(crate) trait WrapExtError {
    type IntoErr: BasicError;

    fn wrap(self, context: &str) -> Self::IntoErr;
}

pub(crate) enum ErrorKind {
    TypeError(TypeError),
    NameError(NameError),
    ParseError(ParseError),
    IoError(IoError),
    Bubble { context: String, contained: Box<ErrorKind> },
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeError(err) => err.fmt(f),
            Self::NameError(err) => err.fmt(f),
            Self::ParseError(err) => err.fmt(f),
            Self::IoError(err) => err.fmt(f),
            Self::Bubble {context: c, contained: err} => write!(f, "{}: {}", c, err),
        }
    }
}

impl From<TypeError> for ErrorKind {
    fn from(value: TypeError) -> Self { ErrorKind::TypeError(value) }
}

impl From<NameError> for ErrorKind {
    fn from(value: NameError) -> Self { ErrorKind::NameError(value) }
}

impl From<ParseError> for ErrorKind {
    fn from(value: ParseError) -> Self { ErrorKind::ParseError(value) }
}

impl From<IoError> for ErrorKind {
    fn from(value: IoError) -> Self { ErrorKind::IoError(value) }
}

impl BasicError for ErrorKind {}

pub(crate) type ResultKind<T> = Result<T, ErrorKind>;
pub(crate) type DataResultKind = ResultKind<DataType>;

pub(crate) fn bubble_result<T, E: BasicError>(r: Result<T, E>, context: &str) -> ResultKind<T> {
    r.map_err(|err| err.bubble(context))
}

pub(crate) struct TypeError {
    pub(crate) context: String,
    pub(crate) expected: EmptyDataType,
    pub(crate) actual: DataType,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: expected {:?}, got {:?}", self.context, self.expected, self.actual)
    }
}

impl BasicError for TypeError {}

pub(crate) type TypeResult<T> = Result<T, TypeError>;
pub(crate) type DataTypeResult = TypeResult<DataType>;

pub(crate) struct NameError {
    pub(crate) context: String,
    pub(crate) name: String,
}

impl Display for NameError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.context, self.name)
    }
}

impl BasicError for NameError {}

pub(crate) struct ParseError {
    pub(crate) context: String,
    pub(crate) message: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.context, self.message)
    }
}

impl BasicError for ParseError {}

pub(crate) struct IoError {
    pub(crate) context: String,
    pub(crate) error: io::Error,
}

impl Display for IoError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.context, self.error)
    }
}

impl BasicError for IoError {}

impl WrapExtError for io::Error {
    type IntoErr = IoError;

    fn wrap(self, context: &str) -> Self::IntoErr {
        IoError { context: context.to_string(), error: self }
    }
}
