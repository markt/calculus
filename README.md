# calculus

We plan to use a module structure for the project. `expressions.hs` will contain all the types, printing functions, and parsers for expressions. `paren.hs` will contain all the general parsers. Generic code will be located in `utilities.hs`. `/test` will contain all the testing code


## laws

```
data Law = Law String Equation
type Equation = (Expr,Expr)
```




## Special Features

### Constants




### Evaluate




### Simplifying Laws





### Less Steps

In writing our laws, we considered the tradeoff between having fewers laws that are more general versus having a greater number of more specific laws that result in fewer steps. For instance, instead of writing two laws to remove zeros from addition expressions, `zeros add: x + 0 = x` and `zeros add2: 0 + x = x`, we could use the law `const on left add: x + p = p + x` to ensure constants always appear on the left then we only need the law `zeros add: 0 + x = x`.

Ultimately, we elected to include more specific laws that result in fewer steps. While having a more concise list of laws is cleaner on our end, our aim is to provide the most useful calculator for the end user. Using a greater number of specific laws enables more succinct calculations, which is in the end users best interest.

