use std::str::FromStr;
use nom::branch::alt;
use nom::bytes::complete::{tag_no_case, take_till1, take_until, take_until1, take_while, is_not, is_a};
use nom::character::complete::{char, i64, multispace0, one_of, u32};
use nom::combinator::{all_consuming, consumed, map, opt, rest, verify};
use nom::{Err, Finish, IResult, ErrorConvert, Parser, InputLength, InputIter};
use nom::bytes::complete::tag;
use nom::error::{Error, ErrorKind, VerboseError};
use nom::error::ParseError as NomParseError;
use nom::multi::separated_list1;
use nom::number::complete::double;
use nom::sequence::{delimited, preceded, terminated, tuple};
use crate::data::{DataType, Expression};
use crate::error::ParseError;
use crate::keyword::Keyword;

const VALID_NAME_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01213456789-_";

// fn trail_ws<'a, O, E: ParseError<&'a str>, F>(mut f: F) -> (impl for<'b> FnMut(&'b str) -> IResult<&'b str, O, E>)
//     where F: Parser<&'a str, O, E>
// {
//     terminated::<&'a str, O, _, E, F, _>(f, multispace0)
// }

fn trail_ws<I, O, E, P>(p: P) -> impl FnMut(I) -> IResult<I, O, E>
    where
        E: NomParseError<I>,
        I: nom::InputTakeAtPosition + nom::InputTake + nom::InputIter + nom::InputLength + Clone,
        <I as nom::InputIter>::Item: nom::AsChar + Clone,
        <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
        P: Parser<I, O, E>,
{
    terminated(p, multispace0)
}


macro_rules! trail_ws {
    ($f:tt) => {
        terminated($f, multispace0)
    };
}

fn punct(input: &str) -> IResult<&str, std::primitive::char> {
    alt((char(','), char(';'))).parse(input)
}

pub(crate) trait Parseable: Sized {
    fn parser(input: &str) -> IResult<&str, Self>;
}

impl Parseable for DataType {
    fn parser(input: &str) -> IResult<&str, Self> {
        alt((
            map(
                delimited(char('"'), take_until("\""), char('"')),
                |x: &str| Self::String(x.to_string()),
            ),
            map(verify(consumed(double),
                       |x: &(&str, f64)| (x.0.contains("."))), |(_, x)| Self::Float(x)),
            map(i64, |x| Self::Integer(x)),
        )).parse(input)
    }
}

impl Parseable for Expression {
    fn parser(input: &str) -> IResult<&str, Self> {
        alt((
            delimited(
                trail_ws(char('(')),
                terminated(Self::parser, multispace0),
                char(')')
            ),
            map(
                tuple((
                    trail_ws(parse_non_compound_expression),
                    trail_ws(alt((
                        tag("+"),
                        tag("-"),
                        tag("*"),
                        tag("/"),
                        tag("^"),
                        tag("=="),
                        tag("<>"),
                        tag("!="),
                        tag("<"),
                        tag("="),
                        tag("<="),
                        tag(">"),
                        tag(">="),
                    ))),
                    Self::parser
                )),
                |(a, op, b)| match op {
                    "+" => Self::Add(a.into(), b.into()),
                    "-" => Self::Subtract(a.into(), b.into()),
                    "*" => Self::Multiply(a.into(), b.into()),
                    "/" => Self::Divide(a.into(), b.into()),
                    "^" => Self::Power(a.into(), b.into()),
                    "=" | "==" => Self::Equals(a.into(), b.into()),
                    "<>" | "!=" => Self::NotEquals(a.into(), b.into()),
                    "<" => Self::LessThan(a.into(), b.into()),
                    "<=" => Self::LessThanEquals(a.into(), b.into()),
                    ">" => Self::GreaterThan(a.into(), b.into()),
                    ">=" => Self::GreaterThanEquals(a.into(), b.into()),
                    _ => unreachable!("This operator should already have been checked"),
                }
            ),
            map(
                DataType::parser,
                |x| Self::Literal(x),
            ),
            map(
                is_a(VALID_NAME_CHARS),
                |x: &str| Self::Variable(x.to_string()),
            ),
        )).parse(input)
    }
}

fn parse_non_compound_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        delimited(
            terminated(char('('), multispace0),
            terminated(Expression::parser, multispace0),
            char(')')
        ),
        map(
            DataType::parser,
            |x| Expression::Literal(x),
        ),
        map(
            is_a(VALID_NAME_CHARS),
            |x: &str| Expression::Variable(x.to_string()),
        ),
    )).parse(input)
}

impl Parseable for Keyword {
    // TODO: allow for whitespace between keywords and expressions
    fn parser(input: &str) -> IResult<&str, Self> {
        alt((
            map(
                all_consuming(multispace0),
                |_| Self::Rem(String::new()),
            ),
            map(
                preceded(
                    tag_no_case("REM "),
                    rest,
                ),
                |x: &str| Self::Rem(x.to_string())
            ),
            map(
                preceded(
                    tag_no_case("LET "),
                    tuple((
                        terminated(
                            trail_ws(is_a(VALID_NAME_CHARS)),
                            trail_ws(char('=')),
                        ),
                        Expression::parser,
                    ))
                ),
                |(name, data)| Self::Let(name.to_string(), data),
            ),
            map(
                tuple((
                    map(
                        alt((tag_no_case("PRINT "), tag_no_case("PRINTLN "))),
                        |x: &str| x.to_lowercase().contains("ln"),
                    ),
                    separated_list1(trail_ws(punct), trail_ws(Expression::parser)),
                )),
                |(newline, x)| Self::Print(newline, x),
            ),
            map(
                preceded(
                    terminated(
                        trail_ws(tag_no_case("GO")),
                        tag_no_case("TO ")
                    ),
                    u32,
                ),
                |x| Self::Goto(x as usize),
            ),
            map(
                preceded(
                    tag_no_case("IF "),
                    tuple((
                        Expression::parser,
                        preceded(
                            tag_no_case(" THEN "),
                            u32,
                        ),
                    )),
                ),
                |(c, line)| Self::IfThen(c, line as usize),
            ),
            map(
                preceded(
                    tag_no_case("FOR "),
                    tuple((
                        terminated(
                            trail_ws(is_a(VALID_NAME_CHARS)),
                            trail_ws(char('=')),
                        ),
                        Expression::parser,
                        preceded(
                            tag_no_case(" TO "),
                            Expression::parser,
                        ),
                        opt(preceded(
                            tag_no_case(" STEP "),
                            Expression::parser,
                        )),
                    )),
                ),
                |(name, start, end, step)| Self::For(
                    name.to_string(), start, end, step, None)
            ),
            map(
                preceded(
                    tag_no_case("NEXT "),
                    trail_ws(is_a(VALID_NAME_CHARS)),
                ),
                |x: &str| Self::Next(x.to_string(), None),
            ),
            map(
                tag_no_case("END"),
                |_| Self::End,
            )
        )).parse(input)
    }
}

impl FromStr for Keyword {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        all_consuming(trail_ws(Keyword::parser))(s).finish()
            .map(|x| x.1)
            .map_err(|err| ParseError {
                context: "Keyword (failed to parse)".to_string(),
                message: format!("{} at {}", err.code.description(), err.input.to_string())
            })
    }
}


