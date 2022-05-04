use std::io;
use std::io::Write;
use crate::data::{DataType, EmptyDataType, Expression};
use crate::error::{BasicError, bubble_result, DataResultKind, ErrorKind, IoError, NameError, ResultKind};
use crate::{ControlType, State};
use crate::error::ErrorKind::TypeError;


pub(crate) enum Keyword {
    Rem(String),
    // Option: sets interpreter/compiler options
    Let(String, Expression),
    Print(bool, Vec<Expression>),
    // Input(Vec<(String, EmptyDataType)>),
    Goto(usize),
    IfThen(Expression, usize),
    For(String, Expression, Expression, Option<Expression>, Option<usize>),
    Next(String, Option<usize>),
    Dim(String, DataType),
    End,
}

impl Keyword {
    pub(crate) fn eval(&self, state: &mut State) -> ResultKind<ControlType> {
        match self {
            Self::Rem(_) => {}
            Self::Let(name, data) => {
                state.vars.insert(name.clone(), bubble_eval(data, state)?);
            }
            Self::Print(newline, exprs) => {
                print!(
                    "{}{}",
                    exprs.iter()
                        .map(|x| bubble_eval(x, state).map(|y| format!("{}", y)))
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
            Self::IfThen(expr, line) => {
                let result = bubble_eval(expr, state)?;
                if let DataType::Boolean(c) = &result
                {
                    if *c {
                        state.line = *line;
                        return Ok(ControlType::NoIncrement);
                    }
                } else {
                    return result.type_error("Eval", EmptyDataType::Boolean)
                        .map_err(|x| x.into());
                }
            }
            Self::For(name, start, end, step, linked_loc) => {
                let (start, end) = (bubble_eval(start, state)?, bubble_eval(end, state)?);
                let step = match (step, &end) {
                    (Some(x), _) => bubble_eval(x, state)?,
                    (None, DataType::Integer(_)) => DataType::Integer(1),
                    _ => DataType::Float(1.0),
                };
                // Because of various weirdness, the first iteration gets skipped unless I do this.
                // TODO: refactor to not need this
                let start = bubble_result(start - step.clone(), "Eval")?;

                for result in [start.clone(), end.clone(), step.clone()].iter() {
                    if !result.is_numeric() {
                        return result.clone().type_error("Eval", EmptyDataType::Number)
                            .map_err(|x| x.into())
                    }
                }
                state.vars.insert(name.clone(), start.clone());
                state.ctx_vars.insert(name.clone(), (
                    end.clone(),
                    step.clone(),
                ));
                if let Some(line) = linked_loc {
                    state.line = *line;
                    return Ok(ControlType::NoIncrement);
                }
            }
            Self::Next(name, linked_loc) => {
                if let Some(line) = linked_loc {
                    if !match (state.vars.get_mut(name), state.ctx_vars.get(name)) {
                        (Some(DataType::Integer(val)), Some((DataType::Integer(end), DataType::Integer(step)))) => {
                            *val += step;
                            ((*val - *end) * step.signum()) > 0
                        }
                        (Some(DataType::Float(val)), Some((DataType::Float(end), DataType::Float(step)))) => {
                            *val += step;
                            ((*val - *end) * step.signum()) > 0.0
                        }
                        // TODO: split this up into mismatched type and invalid type as well as name errors
                        _ => {
                            return Err(ErrorKind::NameError(NameError {
                                context: "Eval (invalid or incorrectly typed variable)".to_string(),
                                name: name.clone(),
                            }))
                        }
                    } {
                        // Jump to immediately after the FOR statement
                        state.line = *line;
                        return Ok(ControlType::Increment);
                    }
                }
            }
            Self::End => {
                return Ok(ControlType::End);
            }
            _ => {}
        }
        Ok(ControlType::Increment)
    }
}

fn bubble_eval(expr: &Expression, state: &State) -> DataResultKind {
    expr.eval(state).map_err(|x| x.bubble("Eval"))
}
