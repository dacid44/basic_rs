# basic_rs
A learning project to build a rudimentary BASIC interpreter in Rust.

There are many different specifications for various versions of the BASIC language. I am currently
working off of the ECMA-55 spec for Minimal BASIC as, though it was withdrawn, it should be similar;
and it is the best-documented version of the Minimal BASIC spec that I can find. The end goal is
somewhere around Minimal BASIC or Dartmouth BASIC, though it should be noted that **this is**
**currently a learning project, and compliance to any particular specification is not the goal.**

## Instructions currently supported:
- `REM` (aka comment/no-op)
- `LET` (variable definitions)
- `PRINT` (printing without newline)
  - `PRINTLN` (not in any spec I can find, prints with a newline)
- `GOTO`/`GO TO` (jumps to a specific line)
- `END` (exits the program)
