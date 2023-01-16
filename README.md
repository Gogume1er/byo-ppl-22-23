# Build Your Own Probabilistic Programming Language

## Install

After cloning the repo, the easiest way to install all the dependencies is via opam:
```
git clone https://github.com/mpri-probprog/byo-ppl-22-23.git
cd byo-ppl
opam install . --deps-only
```

You can then test your installation with a simple:

```
dune build
```

Or try an example with:
```
dune exec ./examples/funny_bernoulli.exe
```

## Realized work

I have implemented an exact enumeration algorithm for discrete probability distributions inside `Basic`.
It use a CPS to compute efficiently the probabilities to avoid the duplication of some computing.
You can find some examples using it in `examples/enumeration.ml`.
The tests may produce some pictures in the folder `plots` which contains graphic representations of the computed distributions.

Moreover, I have added some probabilities distributions in `Distribution`, which are the hypergeometric and the poisson distributions for discrete distributions, and the exponential, gamma and chi2 distributions for continuous distributions.
Due to the lack of complex examples, I didn't implemented test for these new distributions. 
I have also added the computation of the support for the dirac and binomial distributions.

As expected, the enumeration algorithm is not able to compute the exactly the probability distribution of programs which use discrete distributions with infinite domain or continuous distributions, for which the support is not defined in the code anyway.

However, it may exists some examples programs which use only a combination of discrete distributions with finite domain for which the enumeration algorithm fails to compute the distributions.
For instance, the example defined in `Knuth-Yao` is one of them.
It can be explained by the fact that the program is recursive and it exists an infinite trace of this program.
But inference algorithms never deals with this problem because the probability to follow this infinite trace is equal to 0.

The enumeration algorithm can also be very slow in the case of programs involving a large number of random variables, which often have an exponential (in the number of variables) number of paths to explore.

Regarding rejection sampling and importance sampling, the former reaches its limits when the probabilistic program does too many assumptions about its random variables, resulting in an algorithm which could run for a long time in search of "particles" verifying the conditions, while the latter can also be facing the same limitations, but will quickly terminates, even if it will find a disappointing distribution.

## Organization

The `Byoppl` library contains the following modules

- `Distribution`: Library of probability distributions and basic statistical functions.
- `Basic` (TODO): Basic inference with rejection sampling and importance sampling.
- `Infer` (TODO): Inference on Continuation Passing Style (CPS) models.
- `Cps_operators`: Syntactic sugar to write CPS style probabilistic models.
- `Utils`: Missing utilities functions used in other modules.

Examples can be found in the `examples` directory.
