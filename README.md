# calculus

We plan to use a module structure for the project. `expressions.hs` will contain all the types, printing functions, and parsers for expressions. `paren.hs` will contain all the general parsers. Generic code will be located in `utilities.hs`. `/test` will contain all the testing code


## laws

```
data Law = Law String Equation
type Equation = (Expr,Expr)
```