use std::io;
use std::io::Write;
use crate::data::{DataType, Expression};
use crate::error::{ErrorKind, IoError, ResultKind};
use crate::{ControlType, State};


pub(crate) enum Keyword {
    Rem(String),
    // Option: sets interpreter/compiler options
    Let(String, Expression),
    Print(bool, Vec<Expression>),
    Goto(usize),
    Dim(String, DataType),
    End,
}

impl Keyword {
    pub(crate) fn eval(&self, state: &mut State) -> ResultKind<ControlType> {
        match self {
            Self::Rem(_) => {}
            Self::Let(name, data) => {
                state.vars.insert(name.clone(), data.eval(state)?);
            }
            Self::Print(newline, exprs) => {
                print!(
                    "{}{}",
                    exprs.iter()
                        .map(|x| x.eval(state).map(|y| format!("{}", y)))
                        .collect::<ResultKind<Vec<_>>>()?
                        .join(" "),
                    if *newline { "\n" } else { "" },
                );
                if let Err(err) = io::stdout().flush() {
                    return Err(ErrorKind::IoError(IoError {
                        context: "Eval (IO error)".to_string(),
                        error: err,
                    }))
                }
            }
            Self::Goto(line) => {
                state.line = *line;
                return Ok(ControlType::NoIncrement);
            }
            Self::End => {
                return Ok(ControlType::End);
            }
            _ => {}
        }
        Ok(ControlType::Increment)
    }
}
