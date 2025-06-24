# Changelog for `math-dsl`

## 0.6.1 - 2025-06-24

###
- Simplifiy AST over addition within subexpressions
  - E.g., `(1+x+x+2)^2` -> `(2x + 3)^2`
- Fixed CHANGELOG.md order

## 0.6.0 - 2025-06-24

###
- Product simplification now supports implicit multiplication over fractions given numerators and denominators are parenthesised  
  - E.g., `xxy((xy)/y)` -> `(x^3y^2)/y`
- Simplify AST over addition

## 0.5.2 - 2025-06-22

###
- Product simplification over subexpressions (e.g., `(1+xx)^3` -> `(1+x^2)^3`)  
  - For a more complex example, DSL now simplifies `(1+xx)(1+x^2)` -> `(1+x^2)^2`
- Pretty printer now pretty

## 0.5.1 - 2025-06-22

### 
- Generalise product simplification to support repeated full expressions (e.g., `(1+x)`)

## 0.5.0 - 2025-06-22

###
- Count occurrences in a product term so program can 'fold' it during simplification
- Stitch occurrences together to form a folded product
- Simplify the products in an Expr AST

## 0.4.0 - 2025-06-21

###
- Lexer can handle parentheses (including implicit multiplication over paren-delimited expressions)
- Make pretty printer to convert Expr back to a String

## 0.3.1 - 2025-06-21

###
- Fixed bug where implicit Mul wasn't recognised by the lexer

## 0.3.0 - 2025-06-20

###
- Refactored AST to only take two operands rather than a list of expressions
- Refactored parse method to Pratt-parsing
  - Lexer to tokenize strings
  - State monad TokenStream to keep track of tokens while building expressions
  - NUD and LED methods to parse expressions

## 0.2.0 - 2025-06-18

###
- Create Expr AST to represent algebraic expressions
- Parse input from stdin extracting command and expression
- Generate Expr from inputted expression (so far only handles products and fractions with no parentheses)
- Generate Expr over addition

## 0.1.0 - 2025-06-17

###
- Initial project directory setup using Stack
- LICENSE author update