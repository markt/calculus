# Calculus

Welcome to our Calculus calculator!

Our package is a command line tool that takes in user input in the form of expressions and outputs solved expressions. Calculations are performed according to laws specified in `laws.txt`. As a calculation is performed, each intermediate step is shown and the rule applied is listed.



## Usage

After installing the package, simply execute `stack run` to start the program. Then enter in any calculus to solved!

For example, to calculate the derivate of `x ^ 5` by `x` run `d/dx x ^ 5`.



## Testing

To test the package, run `stack test`. Running `stack ghci` allows for the different functions of the package to be tested.



## Package Structure
We use a module structure for the project. `Expressions.hs` contains all the types, printing functions, and parsers for expressions. `Parsing.hs` contains all the general parsers. Generic code is located in `Utilities.hs`. `Printing.hs` contains all the functions used to neatly print calculations. `/test` will contain all the testing code.




## Special Features

### Constants

We hardcoded variables ```p```, ```q``` to represent constants and added associated code to handle constants correctly. Thus, when we write ```d/dx p*x = p```, we know that ```p``` represents a constant value, in our case an ```Int```.


### Evaluate

We added special function ```eval``` to simplify our derivations further. Suppose you're finding a derivative of ```x^5```. Our reasoner correctly deduces ```5*x^(5-1)```. However, this is not a very user friendly way to display the result. So ```eval``` calculates the result of ```5-1``` and inserts it in the place of the original expression. Thus, we get ```5*x^4```.

Additionally, we simplify the parenthesized expressions of the type ```a*(b*(c*(...(n*expr))))```, where ```a, b, c, ..., n``` are constants, to ```prod*expr```, where ```prod = a*b*c*...*n```.


### Shuffled Laws

After endless shuffling around of laws, we decided to play with our law order application by adding an element of randomness. We apply our regular order of laws, once, and then we shuffle the laws within a list ```n``` times, and for each try we redo the calculation. The shortest calculation (as measured by number of steps) gets displayed. Our short testing did not reveal any significant advantages to our method, but we are hopeful that more thorough testing using a greater list of laws on a more complex expression yields a better result.


### Less Steps

In writing our laws, we considered the tradeoff between having fewers laws that are more general versus having a greater number of more specific laws that result in fewer steps. For instance, instead of writing two laws to remove zeros from addition expressions, `zeros add: x + 0 = x` and `zeros add2: 0 + x = x`, we could use the law `const on left add: x + p = p + x` to ensure constants always appear on the left then we only need the law `zeros add: 0 + x = x`.

Ultimately, we elected to include more specific laws that result in fewer steps. While having a more concise list of laws is cleaner on our end, our aim is to provide the most useful calculator for the end user. Using a greater number of specific laws enables more succinct calculations, which is in the end users best interest.


### Further Work

We attempted to implement the application of simplifying laws to expressions before derivative laws are applied but fell short. We tried applying laws to the expression to be derived then applying laws to the full expression with the derivative, but realized our implementation was off as we disregard the steps simplifying the first expressions. To see our progress, take a look at the branch `simp_before_derive`.
