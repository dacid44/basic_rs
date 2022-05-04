#![warn(missing_docs)]
#![doc(html_root_url = "https://dacid44.github.io/basic_rs/")]
//! A learning project to build a rudimentary BASIC interpreter in Rust.
//!
//! There are many different specifications for various versions of the BASIC language. I am
//! currently working off of the ECMA-55 spec for Minimal BASIC as, though it was withdrawn, it
//! should be similar; and it is the best-documented version of the Minimal BASIC spec that I can
//! find. The end goal is somewhere around Minimal BASIC or Dartmouth BASIC, though it should be
//! noted that **this is currently a learning project, and compliance to any particular**
//! **specification is not the goal.**
//!
//! ## Instructions currently supported:
//! |                            |                                                   |
//! |----------------------------|---------------------------------------------------|
//! | `REM`                      | comment/no-op                                     |
//! | `LET`                      | variable definitions                              |
//! | `PRINT`                    | printing without newline                          |
//! | `PRINTLN`                  | not in any spec I can find, prints with a newline |
//! | `GOTO`/`GO TO`             | jumps to a specific line                          |
//! | `IF...THEN`                | conditional execution                             |
//! | `FOR...TO(...STEP)`/`NEXT` | for loops                                         |
//! | `END`                      | exits the program                                 |

mod data;
mod keyword;
mod parser;
mod error;

use std::collections::HashMap;
use std::{env, io};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::process::exit;
use std::str::FromStr;
use dialoguer::Input;
use crate::data::DataType;
use crate::error::{ParseError, ResultKind, WrapExtError};
use crate::keyword::Keyword;

fn main() {
    let mut state = State { vars: HashMap::new(), ctx_vars: HashMap::new(), line: 0 };
    let mut i_reader = match InstructionReader::new(env::args().collect()) {
        Ok(x) => x,
        Err(err) => {
            println!("Error: {}", err);
            exit(1);
        }
    };
    'outer: for i in 0.. {
        i_reader.read_instruction(i);

        while state.line <= i {
            match i_reader.get_instruction(state.line)
                .expect(&*format!("No instruction found at line {}", state.line))
                .eval(&mut state)
            {
                Ok(ControlType::Increment) => { state.line += 1; }
                Ok(ControlType::NoIncrement) => {}
                Ok(ControlType::End) => { break 'outer; }
                Err(err) => {
                    println!("Error: {}", err);
                    state.line += 1;
                }
            }
        }
    }
}

struct State {
    vars: HashMap<String, DataType>,
    ctx_vars: HashMap<String, (DataType, DataType)>,
    line: usize,
}

enum ControlType {
    Increment,
    NoIncrement,
    End,
}

struct InstructionReader {
    interactive: bool,
    instructions: HashMap<usize, Keyword>,
}

impl InstructionReader {
    fn new(args: Vec<String>) -> ResultKind<Self> {
        match args.get(1) {
            Some(arg) => {
                Ok({
                    let mut new_ir = Self {
                        interactive: false,
                        instructions: BufReader::new(
                            File::open(arg)
                                .map_err(|x| x.wrap("ReadProgram (IO error)"))?
                        )
                            .lines()
                            .map(|x| x
                                .map_err(|x| x.wrap("ReadProgramLine (IO error)").into())
                                .and_then(|x| x.parse().map_err(|x: ParseError| x.into()))
                            )
                            .collect::<ResultKind<Vec<_>>>()?
                            .into_iter()
                            .enumerate()
                            .collect()
                    };
                    for i in 0..new_ir.instructions.len() {
                        if matches!(new_ir.instructions.get(&i), Some(Keyword::Next(..))) {
                            if let Err(msg) = match_next_for(i, &mut new_ir.instructions) {
                                // TODO: make this an ErrorKind
                                panic!("Error linking FOR/NEXT: {}", msg);
                            }
                        }
                    }
                    new_ir
                })
            }
            _ => Ok(Self{
                interactive: true,
                instructions: HashMap::new(),
            })
        }
    }

    /// Reads the next instruction from the prompt. If reading from a file, this is a no-op.
    /// There are a couple intricacies with what the value of n should be. Namely, it should be
    /// incremented by 1 on each call.
    /// TODO: refactor so the above is not an issue
    fn read_instruction(&mut self, n: usize) {
        if self.interactive {
            loop {
                let input = Input::<String>::new()
                    .with_prompt(format!("In [{n}]"))
                    .interact_text()
                    .unwrap();
                match input.parse::<Keyword>() {
                    Ok(k) => {
                        self.instructions.insert(n, k);
                        if let Err(msg) = match_next_for(n, &mut self.instructions) {
                            println!("Error linking FOR/NEXT: {}", msg);
                        } else {
                            break;
                        }
                    }
                    Err(err) => { println!("Error: {}", err) }
                }
            }
        }
    }

    /// Get the instruction recorded at the specified line.
    fn get_instruction(&self, i: usize) -> Option<&Keyword> {
        self.instructions.get(&i)
    }
}

fn match_next_for(next_loc: usize, instructions: &mut HashMap<usize, Keyword>) -> Result<(), String> {
    let (next_name, next_linked_loc) = match instructions.get(&next_loc) {
        Some(Keyword::Next(x, y))  if y.is_none() => (x, y),
        _ => {
            return Err("Referenced next_loc was not a valid and unlinked NEXT Keyword".to_string());
        }
    };
    for i in (0..next_loc).rev() {
        if let Some(
            Keyword::For(name, .., linked_loc)
        ) = instructions.get(&i)
        { if linked_loc.is_none() {
            if name != next_name {
                return Err("The found FOR Keyword's control variable does not match that of the linked NEXT Keyword.".to_string());
            }
            // Should be safe as we just checked it
            if let Keyword::For(.., x) = instructions.get_mut(&i).unwrap() {
                *x = Some(next_loc);
            }
            // Should be safe as we checked it at the beginning of the function
            if let Keyword::Next(_, x) = instructions.get_mut(&next_loc).unwrap() {
                *x = Some(i);
            }
            return Ok(())
        }}
    }
    Err("Did not find an unlinked FOR Keyword to link to.".to_string())
}
