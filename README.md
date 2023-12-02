# Assignment 03 - Template Code

## Students

- Andrea Brites Marto
- Alessio Giovagnini

## Bonus logic

We thought a good way to solve the precedence problem, and we came up with following solution:
We implemented an expression binary tree, these are the steps:

- Built a list in infix notation with all symbols (operators and operand).
- Convert the list in infix notation to a list in postfix notation.
- Build the expression tree from the postfix list.
- Evaluate the final responsePredicate by crossing the expression tree
