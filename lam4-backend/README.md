# To set up

1. Use GHC 9.8.2
2. Make sure you have a working Z3 available through PATH. The symbolic evaluator hasn't been built yet, but one of the dependencies, Grisette, may require it already

Note that the parser for the Haskell backend may not be in sync with the latest Langium frontend grammar, in the sense that support for all the constructs in the Langium grammar may not yet have been implemented.