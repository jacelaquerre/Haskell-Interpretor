# The Goal of Our project
The goal of our project was to have an interpreter function with an environment that included the data types pair and tagged union. We also created a typechecker function and tests to check both our functions

# Homework Base
For the base of our project we used the homework number four and L1

# Where Did We End Up
We ended up with interpreter and typchecker functions and tests but no ability to have the testing function run by itself

# Harder? Easier?
The setup of a Haskell project like this was a lot harder than initially thought. It took up a lot of our time and there were a lot of weird bugs and missing files/code that were essential to having a functioning project. Related to that, getting the testing functions to work was also hard due to missed steps in setup and missing code required to run the tests. However, actually creating the cases in the interpreter for our new datatypes, pair and tagged union, was easier than expected and with the math provided made a lot of sense to us after an elaboration on tagged unions.

# How Can You Run Our Tests
We had a lot of issues trying to get tests to run. We think we are close but there is still something wrong with the parser. The error we currently get is:
TESTS
Parse Failure
> line:5 column:29
One of:
<interactive>: <stdout>: commitAndReleaseBuffer: invalid argument (invalid character)
Makefile:17: recipe for target 'eval' failed
make[1]: *** [eval] Error 1
make[1]: Leaving directory 'C:/Users/jacel/Documents/cs225/CS225FinalProject'
Makefile:56: recipe for target 'fp' failed
make: *** [fp] Error 2
