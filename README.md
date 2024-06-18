# λ4

This is a kind of rational reconstruction of the 'MathLang' dialect of L4. That is, it is, at its core, a functional expression language; but it aims to extend the MathLang work by incorporating solver / automated reasoning capabilities. It also differs from Natural L4 in offering a textual format with a more constrained surface syntax (with a 'no-code,' intuitive GUI for non-programmers on the roadmap).

## Goals in the short term

* things will be rough around the edges, and technical debt will be unavoidable --- but we'll be explicit about said debt
* we'll try to do some upfront software design, especially for the things that are harder to change down the road

## In the mid to longer term

The goal is to get something that

* is more *modular*, and hence *maintainable*, with less technical debt, to enable rapid prototyping and iteration
* is more *extensible* -- e.g., with things like metadata
* has a *textual* format (potentially in addition to a visual one, if time/energy permits), and hence plugs better into other PL-related systems (eg assistive technology that uses structural editing frameworks)
* emphasizes *usability*, *ease of learning*, and *ergonomics*, in particular, support for *tooling* and other modern conveniences to make working with L4 less onerous and more intuitive
* allows you not only to *evaluate expressions* (via a 'MathLang'-esque expression sub-language), but also to deploy *formal-methods-powered techniques* (e.g., to explore or understand the contract better, to check that your formalization of the contract does indeed, or even to synthesize or repair L4 specifications).

## Noteworthy TODOs

### Backlog

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

## The various fragments / sub-languages of Mini L4

### The type declarations / data modelling sub-language

This differs from the current JSON transpiler in that

* it has more support for *metadata*, whether as 'general,' schema-global metadata, or metadata that's specific to a property
* it supports open sums and union types
* it's textual
* it's not afraid to look a bit more like a conventional PL, since the people writing the data models will typically have programming experience, and since it wouldn't be hard to add support for things that help explain the data model -- e.g. intuitive visualizations and a 'preview mode' that explains the data model in more ordinary natural language terms
  * [TODO] plug into visualizations / a diagrammatic UI that'll help with understanding the data model
  * [MAYBE] along with a GUI for editing or crafting a data model from scratch?

## Langium notes

To factor a grammar into multiple sub-grammars: <https://github.com/eclipse-langium/langium/tree/main/examples/requirements>

## Things to think about

### How to get interop --- in particular, how to enabling interfacing with L4 from other languages

1. Have one L4 runtime; put it behind a REST API or use some sort of RPC (and maybe also offer more ergonomic wrappers around http or rpc clients, e.g. in the same way that [the OpenAPI python lib](https://github.com/openai/openai-python) basically wraps their REST API; c.f. also things like vscode rpc)

2. If the runtime is in Haskell: use the FFI (e.g. go through C).

* <https://www.reddit.com/r/haskell/comments/wighew/haskell_language_interoperability_and_how_to/>
* See <https://www.reddit.com/r/haskell/comments/17ec6bu/how_to_expose_haskell_application_to_other/> and e.g. <https://github.com/simplex-chat/simplex-chat/blob/4000fe383d6563509555d5d51a0b5f116e0fb363/apps/multiplatform/common/src/commonMain/cpp/desktop/include/jni.h#L214>

### Using automated reasoning to help simplifying statues / legalese, via checking equivalences

An example of a convoluted piece of regulation: the US Tax Code, in particular I.R.C. § 163, which is about the deductibility of interest.

Quote from "Constituencies and Control in Statuory Drafting":

> That statute states a broad general rule: “[t]here shall be allowed as a deduction all interest paid or accrued within the taxable year on indebtedness.”56 But, much farther down in the statute, it creates a large exception: § 163(h)(1) states that “personal interest” is not deductible, thereby excepting personal interest from the ambit of the general rule. Making matters more complicated, § 163(h)(2) then creates an exception to that exception: It defines “personal interest” to exclude certain items including, significantly, “qualified residence interest." This is the basis for the home mortgage interest deduction.

> The entire disallowance of personal interest in §163(h) threatens to swallow the broad general rule in § 163(a) that all interest is deductible, and in fact substantially changes the principle stated in the general rule to the point where one wonders why the broad general rule was necessary. Moreover, the exception to the exception in § 163(h)(2) buries the intricate and important rules allowing home mortgage interest deductions deep within the statute

> The statute could, alternatively, be written using a much narrower rule that is circumscribed using a defined term, while achieving the same substantive outcome.  For example:

> **Narrow Rule**: Approved Business Interest and Approved
Personal Interest are deductible.

> **Defined Term 1**: Approved Business Interest includes ___.

> **Defined Term 2:** Approved Personal Interest includes ___.

> In short, rather than stating a broad general rule that all interest is deductible followed by large exceptions, the statute could have articulated a narrower rule stating that only certain “approved interest” is deductible, and then could have defined that term.

IDEA: Having a way to automatedly checking the logical (or 'practical / outcome') equivalence of these two formulations would presumbly be helpful, as it was with Forge. We should try to support that.

## Inspiration / further reading

### On keeping the underlying language grammar relatively constrained, but still understandable to non-professional-programmers via UI/UX

* [The Enso Language](https://github.com/enso-org/enso), a language for data processing, seems to also [subscribe to a lot of the same tenets](https://github.com/enso-org/enso/blob/develop/docs/enso-philosophy.md), e.g.
  * "Explicit Over Implicit: The design of Enso and its libraries should *always* account for making all behaviour explicit."
  * "Unified Syntax: The language syntax should be simple, concise, and be usable on both the type and term levels.
  * "Choice in a language inherently creates myriad ways to do things. This is singularly unhelpful in any programming language, let alone one intended to span all experience levels, as it increases the cognitive burden of using the language."
