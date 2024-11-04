# README

This is a very simple implementation of what I take to be the essence of "Contract Automata: An Operational View of Contracts Between Interactive Parties", with some twists from Camilieri's SCL.

I did this mainly for my own understanding.

## Where I've departed from the paper

* no interleaving -- just synchronous composition right now
* no sequence operator (that seems to be sugar anyway)
* Didn't bother explicitly supporting non-top-level conjunctions, to keep the implementation simple (these can be simulated via other, less ergonomic ways)
* Simplified event model - single events rather than sets of concurrent events
* My handling of conditional (If) clauses might differ from the paper's

## What to look at

See [`Examples.hs`](./src/Examples.hs) for some examples, e.g.

1. A basic contract interaction example (from p.31 of the paper)
2. The bank example from the paper that gets you 'conflicting clauses'

In particular, you can try running things like the following in your terminal:

```bash
cabal repl
:l Examples
runPage31AutOnGivenTrace
```

## To do / to think about

* How would interleaving work? Do we even need that?
* In a more realistic implementation, would want to be able to query / write predicates on what obligations / permissions are currently active
* explicit support for deadlines
* Whether this can be extended to get us model finding or model checking
