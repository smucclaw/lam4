# Proposal / RFC for normative constructs in L4 (Aug 26 2024)

This proposal advances an alternative grammar for normative constructs in L4. The grammar I'll propose is meant to be closer to abstract syntax rather than the surface syntax, though I will also supply examples of the corresponding surface syntax; it is also meant to start a conversation, rather than be the last word on this (e.g., it's lacking further sugar and combinators).

Let me start by reviewing what I take to be the current syntax for expressing normative things in L4.

## The current L4 syntax, for comparison

The core of it is something like this:

```current_l4
<Agent> MUST/MAY ...
(HENCE ...)?
(LEST ...)?
```

where the question marks indicate that the construct is optional.

## An alternative syntax

I'll present an alternative syntax before advancing some arguments for it.

### The alternative, in its abstract form

The alternative I want to present is, in its more abstract Haskell data-type form, something like this:

```haskell

data Statement
  = IfStmt Expr (NonEmpty Statement) [Statement] -- If Condition Then Otherwise
  | Assign Name Expr
  | Action Name [Name] [Statement]               -- Action NameOfAction Params Body(block of statements)
  | Norm   Deontic
  | Breach
  -- there could be more sugar; I'm just trying to get at the core idea for now
  deriving stock (Show, Eq, Ord)

data Deontic =
  MkDeontic { name         :: Maybe Name
              -- Users can supply a name for the deontic if it doesn't appear in the scope of another Statement that already has a Name
            , agent        :: Name -- In the future may want to be able to quantify over agents too
            , deonticModal :: DeonticModal
            , action       :: Statement
            -- , deadline     :: WIP_NotSureYet -- only in v2 / v3
            }
  deriving stock (Eq, Show, Ord)
```

and where the expression language fragment basically looks like what you'd expect, plus sugar for things like predicates over normative stuff:

```haskell
data Expr
= IfThenElse Expr Expr Expr -- differs from the statement version in tt this is an expression
| ...                       -- the other things you'd expect
| NormIsInfringed Name      
  -- NormIsInfringed NameOfNorm.
  -- Sugar for a predicate checking if @nameOfNorm@ is violated (users can supply unique identifiers for Deontics and IfThenOtherwise statements that contain a Deontic)
```

#### The basic intuition

I'll get to why I prefer this grammar over the current L4 grammar later, but for now, the basic intuition behind this syntax is this. One way of adding normative stuff to a functional expression language amounts to augmenting the evaluator with a Store / EvalState; and in particular, keeping track of the statuses of the normative stuff (e.g. what obligations are operative at any point; what obligations have been satisfied or violated; what actions have been taken), as well as whatever other state is required.

For this reason, it's natural to model normative stuff as *statements* instead of *expressions*.
Thinking of them as statements also agrees, not just with work in AI and AI x Law, but also with a longstanding tradition in philosophy and linguistics that models them as being fundamentally *non*-truth-conditional, stateful things (e.g., as 'proposals to update the conversational scoreboard').

### Examples of corresponding surface syntax

The abstract syntax might be hard to understand. Here are some quick examples of what a corresponding surface syntax could look like.

You can play with the corresponding surface syntax by [pasting the Langium grammar at this link](../lam4-frontend/src/language/lam4.langium) into the [Langium playground](https://langium.org/playground/). Scoping for records won't currently work in the playground (or in the current implementation), but it's good enough to get a sense for what a potential corresponding service syntax would look like.

Please don't try to run these examples on the current Lam4 toolchain just yet -- the latest changes to the Langium grammar have not been integrated into the typechecker and Parser.hs yet.

#### A simple bike sale example

```lam4
// this uses Sigs, but can be refactored to use records --- don't get hung up on this part
ONE CONCEPT Buyer {}
ONE CONCEPT Seller {}

ACTION `pay for bike`
ACTION `transfer bike to Buyer`

IF        Buyer `pay for bike`
THEN      Seller MUST `transfer bike to Buyer`
OTHERWISE Breach!
```

#### A more complicated example that demonstrates built-in predicates like `IS_INFRINGED`, as well as references to norms

```lam4
// Again, ignore the 'ONE CONCEPT' stuff -- focus on ACTIONs and the other normative stuff
ONE CONCEPT Buyer {}
ONE CONCEPT Seller {}
ONE CONCEPT Courier {}

ACTION `pay for bike`
ACTION `transfer bike to Courier within two days`
ACTION `ferry bike to Buyer within three days`

§1
IF    Buyer `pay for bike`
THEN  Seller MUST `transfer bike to Courier within two days`

§2: BikeTransportRule
IF   Seller `transfer bike to Courier within two days`
THEN Courier MUST `ferry bike to Buyer within three days`

IF   BikeTransportRule IS_INFRINGED
THEN Breach!

// -- or, if we want to just talk in terms of events / actions:
// IF NOT (Courier `ferry bike to Buyer`)
// THEN Breach!
```

## Why go for this alternative

To clarify, the biggest, most fundamental differences between this alternative syntax and the current L4 syntax are as follows.

* Instead of being forced to write `HENCE/LEST` after a norm, you can talk about what happens if the norm is violated, or if this or that action happens, *in a separate construct*. You can still articulate reparations for an obligation right after declaring the obligation; it's just that you now *also* have the option to articulate reparations in a separate clause.

* There is no longer a `... MAY ... HENCE ...` construct. Instead, the way to express that would be via something like

```lam4
// You don't really need the MAY either, since an absence of a prohibition to phi implies that you may phi.
Agent MAY take_action

If Agent take_action
THEN ...
```

I think this alternative syntax is better for two reasons.

### First, it's easier to understand for humans

#### The current L4 way of forcing a `HENCE/LEST` after a permission is counterintuitive

To see why, consider how we do *not*, in ordinary English, say things like, 'you may do this work; thus, you will get a bonus'. We would instead just say: 'if you do this work, you will get a bonus'. The alternative syntax I'm proposing is closer to ordinary English in this regard.

It does not seem like a coincidence that most of the other related work on normative DSLs that I've surveyed do not force (their equivalents of a) `HENCE/LEST` after a permission in this way either.

We do say things like 'If you *don't* do this, you will be punished', but the alternative syntax allows for that.

It is also worth noting that in the past, when I surveyed the 3-4 law student interns we had for what they thought the `MAY/MUST ... HENCE/LEST` stuff meant, virtually none of them were able to arrive the correct answer.

#### Having it be in the form of an IF ... THEN ... OTHERWISE is also independently easier to learn and understand

For example, once someone has learned the expression version of an if then else, it's not that much of a conceptual leap from that to a `IF ... THEN ... OTHERWISE` statement.

And the normative DSL SLEEC, which has constructs of the `when <trigger> then <response>` variety, claims that this is easy for non-technical users to understand.

### Second, it is more compositional and modular

For example, jursidication A and jurisdication B agreed that there's an obligation to do something, but disagreed in what the penalties would be, there is a natural, modular way to encode that on the alternative syntax. You would declare the obligation, and then, e.g., encode that each of the jurisdictions had different reparations for that obligation in separate `IF ... THEN ...` statements.

```lam4
// File 1
... MUST NOT spit_gum
```

```jursidiction_A
// import File 1 obligations
IF ... spit_gum
THEN MUST pay_1000_fine
```

```jursidiction_A
// import File 1 obligations
IF ... spit_gum
THEN MUST pay_50_fine
```

By contrast, the current L4 syntax does not give you natural way to represent this; in particular, that both jursidictions in some sense forbid *the same thing*, but differ in their penalties for it. You would have to encode this with *distinct* obligations and reparations, e.g.

```jurisdiction_A
... MUST NOT spit_gum
LEST pay_1000_fine
// I don't know if the current L4 AST even allows you to do a MUST NOT in this way though
```

```jurisdiction_B
... MUST NOT spit_gum
LEST pay_50_fine
```

This means, in other words, that it'll be harder to do things like mixing contract components on the current L4 syntax.

## References

[TODO]
