# Changelog for `math-dsl`

## 0.1.0 - 2025-06-17

###
- Initial project directory setup using Stack
- LICENSE author update

## 0.2.0 - 2025-06-18

###
- Create Expr AST to represent algebraic expressions
- Parse input from stdin extracting command and expression
- Generate Expr from inputted expressed (so far only handles products and fractions with no parantheses)
- Generate Expr over addition

## 0.3.0 - 2025-06-20

###
- Refactored AST to only take two operands rather a list of expressions
- Refactoring parse method to Pratt-parsing
  - Implemented lexer to tokenize strings
  - Implemented State monad TokenStream to keep track of tokens while building expressions
  