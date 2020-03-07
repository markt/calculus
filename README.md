# Calculus

We plan to use a module structure for the project. `expressions.hs` will contain all the types, printing functions, and parsers for expressions. `paren.hs` will contain all the general parsers. Generic code will be located in `utilities.hs`. `/test` will contain all the testing code


## Laws

```
data Law = Law String Equation
type Equation = (Expr,Expr)
```




## Special Features

### Constants

We hardcoded variables ```p```, ```q``` to represent constants and added associated code to handle constants correctly. Thus, when we write ```d/dx p*x = p```, we know that ```p``` represents a constant value, in our case an ```Int```.


### Evaluate

We added special function ```eval``` to simplify our derivations further. Suppose you're finding a derivative of ```x^5```. Our reasoner correctly deduces ```5*x^(5-1)```. However, this is not a very user friendly way to display the result. So ```eval``` calculates the result of ```5-1``` and inserts it in the place of the original expression. Thus, we get ```5*x^4```.

Additionally, we simplify the parenthesized expressions of the type ```a*(b*(c*(...(n*expr))))```, where ```a, b, c, ..., n``` are constants, to ```prod*expr```, where ```prod = a*b*c*...*n```.


### Shuffled Laws

After endless shuffling around of laws, we decided to play with our law order application by adding an element of randomness. We apply our regular order of laws, once, and then we shuffle the laws within a list ```n``` times, and for each try we redo the calculation. The shortest calculation (as measured by number of steps) gets displayed. Our short testing did not reveal any significant advantages to our method, but we are hopeful that more thorough testing using a greater list of laws on a more complex expression yields a better result.


### Simplifying Laws





### Less Steps

In writing our laws, we considered the tradeoff between having fewers laws that are more general versus having a greater number of more specific laws that result in fewer steps. For instance, instead of writing two laws to remove zeros from addition expressions, `zeros add: x + 0 = x` and `zeros add2: 0 + x = x`, we could use the law `const on left add: x + p = p + x` to ensure constants always appear on the left then we only need the law `zeros add: 0 + x = x`.

Ultimately, we elected to include more specific laws that result in fewer steps. While having a more concise list of laws is cleaner on our end, our aim is to provide the most useful calculator for the end user. Using a greater number of specific laws enables more succinct calculations, which is in the end users best interest.

