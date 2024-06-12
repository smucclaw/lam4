# Mini L4 / Textual L4 (Natural L4's illicit child...)

This is meant to be a textual, more constrained version of Natural L4 --- in particular, of the 'MathLang' dialect. The goal is to get something that

* is more *modular*, with less technical debt, to enable rapid prototyping and iteration
* is more *extensible* -- e.g., with things like metadata
* is more *maintainable* (follows from being more modular)
* is *textual*, and hence plugs better into other PL-related systems (eg assistive technology that uses structural editing frameworks)
* emphasizes support for *tooling* and other modern conveniences to make working with L4 less onerous

## The type declarations / data modelling sub-language

This differs from the current JSON transpiler in that

* it has more support for metadata, whether as 'general,' schema-global metadata, or metadata that's specific to a property
* it supports sum types and union types
* it's textual
* [TODO] will plug into visualizations / a diagrammatic UI that'll help with understanding the data model
* [MAYBE] along with a GUI for editing or crafting a data model from scratch?
