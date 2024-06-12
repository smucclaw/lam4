# Mini L4

This is meant to be a textual, more constrained version of Natural L4 --- in particular, of the 'MathLang' dialect. The goal is to get something that is

* smaller and hence easier to prototype with
* more *extensible* -- e.g., with things like metadata
* more *maintainable*
* *textual*, and hence plugs better into other PL-related systems (eg assistive technology that uses structural editing frameworks)

## The type declarations / data modelling sub-language

This differs from the current JSON transpiler in that

* it has more support for metadata, whether as 'general,' schema-global metadata, or metadata that's specific to a property
* it's textual
* [TODO] will plug into visualizations / a diagrammatic UI that'll help with understanding the data model
* [MAYBE] along with a GUI for editing or crafting a data model from scratch?
