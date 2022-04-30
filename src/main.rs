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
    let mut state = State { vars: HashMap::new(), line: 0 };
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
                Ok(Self {
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
                })
            }
            _ => Ok(Self{
                interactive: true,
                instructions: HashMap::new(),
            })
        }
    }

    fn read_instruction(&mut self, n: usize) {
        /// Reads the next instruction from the prompt. If reading from a file, this is a no-op.
        /// There are a couple intricacies with what the value of n should be.
        /// TODO: refactor so the above is not an issue
        if self.interactive {
            loop {
                let input = Input::<String>::new()
                    .with_prompt(format!("In [{n}]"))
                    .interact_text()
                    .unwrap();
                match input.parse::<Keyword>() {
                    Ok(k) => {
                        self.instructions.insert(n, k);
                        break;
                    }
                    Err(err) => { println!("Error: {}", err) }
                }
            }
        }
    }

    fn get_instruction(&self, i: usize) -> Option<&Keyword> {
        self.instructions.get(&i)
    }
}
