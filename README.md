# λ4

A legal DSL that is, at its core, a functional expression language, but that also aims, in the long run, to

* incorporate solver / automated reasoning capabilities, so that you can ask questions like, what needs to happen for me to be able to make this insurance claim (probably coming soon)
* offer a 'low-code', intuitive GUI with an AI (+ PL / program synthesis / formal methods methods)-powered Copilot (on the long-term roadmap)

## How to compile Lam4 files

Use the `lam4-cli`.

EG, from the root `lam4` dir: 

> `cabal run lam4-cli -- examples/arithmetic.l4`

## How to build

This will be simpler soon enough, once I've set up a Nix environment etc. But in the meantime:

You basically have to set up two things:
1. The Langium frontend: see the detailed instructions below
2. The Langium backend:  see the detailed instructions in the `lam4-backend` readme

There will be wrapper scripts / services to make working with this more ergonomic soon enough. But in the very short term, you have to generate the AST from the lam4 frontend, and then run it through the backend parser.

## Vs. main branch L4

This is a kind of rational reconstruction of the 'MathLang' dialect of L4. That is, it is, at its core, a functional expression language; but it aims to extend the MathLang work by incorporating solver / automated reasoning capabilities. It also differs from Natural L4 in offering a textual format with a more constrained surface syntax (with a 'no-code,' intuitive, AI-augmented GUI for non-programmers on the roadmap).

## Goals and Principles, in the mid to longer term

The goal is to get something that

* is more *modular*, and hence *maintainable*, with less technical debt, to enable rapid prototyping and iteration
* is more *extensible* -- e.g., with things like metadata
* has a *textual* format (potentially in addition to a visual one, if time/energy permits), and hence plugs better into other PL-related systems (eg assistive technology that uses structural editing frameworks)
* de-emphasizes complicated parsing; aims to shifts more of the burden instead to UI/UX, AI, program synthesis, etc
* emphasizes *usability*, *ease of learning*, and *ergonomics*, in particular, support for *tooling* and other modern conveniences to make working with L4 less onerous and more intuitive. This is worth emphasizing: the current discourse on L4 usability tends to conflate (i) how easy / intuitive it is to learn it with (ii) how approachable it looks on the surface. In particular, the current approach seems to be to try to disguise the complexity inherent in formalizing law, in a way that can be misleading. By contrast, the Lam4 approach aims to avoid misleading simplification, and instead tries to engender genuine understanding.
* allows you not only to *evaluate expressions* (via a 'MathLang'-esque expression sub-language), but also to deploy *formal-methods-powered techniques* (e.g., to explore or understand the contract better, to check that your formalization of the contract does indeed, or even to synthesize or repair L4 specifications).

## Understanding the grammar / working with it

If you don't need to use record field access or predicate declarations, you can paste the grammar into the playground (https://langium.org/playground/) to see the generated parser in action.

### To build / generate the Langium frontend parser

If you do need either of those:

```bash
npm install
npm run langium:generate
npm run build
```

### Working with it

After building:

* To see what the grammar looks like, you can **generate a railroad diagram from VSCode**, using the Langium VSCode extension.
* To print to std out and save to disk a **human-readable version of the concrete syntax** (i.e., a version without the metadata): `node ./bin/cli toMinimalAst <lam4program filename>.l4`
* To do that for a version of the concrete syntax that includes source text metadata: `node ./bin/cli toAstWithSrcMetadata <lam4program filename>.l4`
  * Note that this is still more like concrete than abstract syntax --- I hope to be able to do that desugaring / simplfiying within the next 1-1.5 weeks. I'll aim to target Andres' AST for concrete evaluation, but I'll probably also experiment with another version on the side that has a few more constructs for symbolic execution.
* To **generate a VSCode extension**: `vsce package`

## Noteworthy TODOs

### Backlog

#### Typechecker, evaluator, backend related

The type checker is currently implemented in Typescript, but in the medium term I want to just move that to the Haskell backend. 

I also want to have some kind of JSON RPC going with the Haskell backend, so that we'll be able to, e.g., parse the program, run the evaluator, and see nice feedback in a webview, all within the IDE.


#### Parser

1. Try cutting down on symbols like braces if this is something that would confuse non-technical users

Right now the data modelling sublanguage uses braces to disambiguate scope.

```mini-l4
GENERAL_METADATA_FOR_TYPES {
  description: "A reasonably simple example of a (mini-)L4 data model"

  nestedMetadata: {
    nestedKey: true
  }
}
```

When time permits -- e.g., shortly after the first end-to-end system is done --- we should try cutting down on symbols like braces if that will confuse non-professional programmers. EG, we could instead go for something that uses indentation:

```mini-l4
GENERAL_METADATA_FOR_TYPES:
  description: "A reasonably simple example of a (mini-)L4 data model"

  nestedMetadata:
    nestedKey: true
```

This is already possible with Langium; I just haven't done it because it requires a bit more effort and would complicate the codebase more than I would like at this stage.

