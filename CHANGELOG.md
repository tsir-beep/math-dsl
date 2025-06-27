# Changelog for `math-dsl`

## 0.10.0 - 2025-06-28
- Proper identity operation mapping
- Differentiate expressions (including partial differentiation)

## 0.9.0 - 2025-06-27
- Negative number support

## 0.8.2 - 2025-06-27
- Updated pretty printer and how it prints fractions
- Function simplify composes simplifyFractions
- Updated DEBUG print

## 0.8.1 - 2025-06-27

###
- Evaluate expressions to 2 d.p
- Idmap for Pow

## 0.8.0 - 2025-06-27

###
- Evaluate expressions with given variable map

## 0.7.0 - 2025-06-27

###
- Factor an expression
  - E.g. `2x(x+1) + (4(x+1)/(y)) + 6(x+1)(x+2)` -> `2(x+1)`

## 0.6.2 - 2025-06-24

###
- Fixed bug which converts Mul to Add during fraction simplification
- Fixed bug which didn't simplify some simpler fractions
- Simplify expressions over operations containing their respective identity element
  - E.g., `x + 0` = `x`
  - E.g., `1x` = `x`
- Fixed pretty printer bug where subexpressions under Mul don't paranthesise 

## 0.6.1 - 2025-06-24

###
- Simplifiy AST over addition within subexpressions
  - E.g., `(1+x+x+2)^2` -> `(2x + 3)^2`
- Fixed CHANGELOG.md order
- Simplify fractions
  - Both expressions and pure numerical fractions

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