# Mini L4 / Textual L4

This is meant to be a textual, more constrained version of Natural L4 --- in particular, of the 'MathLang' dialect.

## In the short term

* things will be rough around the edges, and technical debt will be unavoidable --- but we'll be explicit about said debt
* we'll try to do some upfront software design, especially for the things that are harder to change down the road

## In the mid to longer term

The goal is to get something that

* is more *modular*, and hence *maintainable*, with less technical debt, to enable rapid prototyping and iteration
* is more *extensible* -- e.g., with things like metadata
* has a *textual* format (potentially in addition to a visual one, if time/energy permits), and hence plugs better into other PL-related systems (eg assistive technology that uses structural editing frameworks)
* emphasizes *usability*, *ease of learning*, and *ergonomics*, in particular, support for *tooling* and other modern conveniences to make working with L4 less onerous and more intuitive
* allows you not only to *evaluate expressions* (via a 'MathLang'-esque expression sub-language), but also to deploy *formal-methods-powered techniques* (e.g., to explore or understand the contract better, to check that your formalization of the contract does indeed, or even to synthesize or repair L4 specifications).

## The type declarations / data modelling sub-language

This differs from the current JSON transpiler in that

* it has more support for metadata, whether as 'general,' schema-global metadata, or metadata that's specific to a property
* it supports sum types and union types
* it's textual
* it's not afraid to look a bit more like a conventional PL, since the people writing the data models will typically have programming experience, and since it wouldn't be hard to add support for things that help explain the data model -- e.g. intuitive visualizations and a 'preview mode' that explains the data model in more ordinary natural language terms
  * [TODO] plug into visualizations / a diagrammatic UI that'll help with understanding the data model
  * [MAYBE] along with a GUI for editing or crafting a data model from scratch?

## Inspiration / further reading

### On keeping the underlying language grammar relatively constrained, but still understandable to non-professional-programmers via UI/UX

* [The Enso Language](https://github.com/enso-org/enso), a language for data processing, seems to also [subscribe to a lot of the same tenets](https://github.com/enso-org/enso/blob/develop/docs/enso-philosophy.md), e.g.
  * "Explicit Over Implicit: The design of Enso and its libraries should *always* account for making all behaviour explicit."
  * "Unified Syntax: The language syntax should be simple, concise, and be usable on both the type and term levels.
  * "Choice in a language inherently creates myriad ways to do things. This is singularly unhelpful in any programming language, let alone one intended to span all experience levels, as it increases the cognitive burden of using the language."
