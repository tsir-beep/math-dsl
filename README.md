# math-dsl

A Haskell-based symbolic math DSL for simplifying, factoring, evaluating and differentiating algebraic expressions over simple arithmetic.

## How it works

This program uses a CLI (refer to `Commands` subsection for detailed use) taking a raw string input and converting it into 
an Abstract Syntax Tree (AST) via Pratt Parsing (to consider operation precedence). It can then simplify expressions (folding
products, adding like-terms, simplifying fractions) find the greatest common factor in the whole expression, evaluate
the expression by subbing in variables and even differentiate.

To build the project, make sure you have GHC with Stack. Then simply run `stack build` to build the executable then run
`stack exec math-dsl` to run the program.

## Input

The input is in the format `<COMMAND> <EXPRESSION>`
- E.g. `SIMPLIFY xy + 2xy^2z`

The DSL supports addition (`+`), multiplication (implicit), fractions (`/`) and exponents (`^`). So you can give expressions like
- `x + y + 2`
- `(x+y)^3 + x^2z`
- `xyz + 5x/(y+1) + 3(x+1)^2`

We should acknowledge the ambiguity of providing text inputs for algebraic expressions around fractions. For instance
`x/y` is fine and non-ambiguous but something like `x+1/y+2` can semantically be interpreted as `x + 1/y + 2` or `(x+1)/(y+2)`
Hence, I preface that you should add parantheses where ambiguity may arise (so write as `(x+1)/(y+2)`), even if the fraction
contains a chained product term like `xy`.

## Commands

### SIMPLIFY
This will simplify the entire expression by:
- Folding product terms (carried over addition)
- Add the like terms
- Simplify fractions
  - This includes simplifying the numbers in the numerator and denominator via GCF
- Discard identity elements over their respective operation
  - `Expr + 0` = `Expr`
  - `1Expr` = `Expr`
  - `Expr/1` = `Expr`
  - `Expr^1` = `Expr`

### FACTOR
This will return the Greatest Common Factor (GCF) in an expression i.e., factor the whole expression
- E.g., `xyz + x^2y` has GCF `xy` i.e., can be factorised as `xy(z + x)`
- E.g., `(x+1)^2 + (x+1)(x+2) + ((x+1)/y)` has GCF `(x+1)` i.e., can be factorised as `(x+1)((x+1) + (x+2) + 1/y)`

### EVAL(`<varMap>`)
This will evaluate an expression with a given variable map. `<varMap>` is in the format of `<var>=<number>,...`
- E.g., `EVAL(x=2, y=3, z=4)` (whitespace is not necessary)

### DIFF(`<var>`)
This will differeniate an expression with respect to `<var>` (yes this means it does partial differentiation as well)
- E.g., `DIFF(x) x^2 + x`