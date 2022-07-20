# bdd - Binary Decision Diagram in OCaml

First of all, this project proposes an OCaml implementation of BDD [Binary Decision Diagram](https://en.wikipedia.org/wiki/Binary_decision_diagram), a classic data structure.\
Next, there is an implementation of BDD compression in ROBDD (Reduced Ordered Binary Decision Diagram)
by detecting isomorphic trees via Lukasievicz words.\
Finally, there is a compression ratios analysis because the ROBDD structure depends on the variables order.

This study is based on the research publication [A Theoretical and Numerical Analysis of the Worst-Case Size of Reduced Ordered Binary Decision Diagrams](https://www.researchgate.net/publication/328016560_A_Theoretical_and_Numerical_Analysis_of_the_Worst-Case_Size_of_Reduced_Ordered_Binary_Decision_Diagrams).

*This project is part of my professional training, algorithms course.*

## Execution and experimentation

The Makefile contains all useful commands, especially compile and run.

```console
make run
```

The run command performs the following experiments (see main.ml file):

* BDD and ROBDD from 38 truth table decomposition (3 variables)
* BDD and ROBDD from big integer truth table decomposition (6 variables)
* scalability complexity (display of statistical data arrays)
  * 4 variables > 2^(2^4)=2^16=65.536 combinaisons (OK)
  * 5 variables > 2^32=4.294.967.296 combinaisons (KO!)

To see the data structures and associated compressions, [.dot files](https://graphviz.org) are generated.
