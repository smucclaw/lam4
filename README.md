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

### IDE protocol / synchronizing state between, e.g., a textual editor and a diagrammatic one

* <https://matklad.github.io/2023/10/12/lsp-could-have-been-better.html>
  * "what I think is the biggest architectural issue with LSP [...] LSP is an RPC protocol — it is formed by “edge triggered” requests that make something happen on the other side. But this is not how most of IDE features work. What actually is needed is “level triggered” state synchronization. The client and the server need to agree what something is, deciding the course of action is secondary. It is “to be or not to be” rather than “what is to be done”."
* <https://htmlpreview.github.io/?https://github.com/dart-lang/sdk/blob/8e6a02d899ef62ef5b8405518b36340e609198e2/pkg/analysis_server/doc/api.html>
* <https://www.codemag.com/Article/1811091/Building-a-.NET-IDE-with-JetBrains-Rider>

### Concrete syntax

* [Tonto](https://matheuslenke.github.io/tonto-docs/)'s syntax choices are helpful -- they seemed to have put quite a bit of effort into finding intuitive names, and their language was "designed to allow transformation to a number of languages including UML (more specifically OntoUML), OWL (for gUFO-based ontologies), Alloy" etc
  * Think about whether to use `SPECIALIZES` instead of `SUBSET_OF`, and whether to use `KIND` or `TYPE` instead of `SIG`

### Visualizations and Explainability

Useful links:

* [Eclipse Graphical Language Server Platform for next-generation diagram editors (GLSP)](https://eclipse.dev/glsp/)

* [Tonto - A Textual Syntax for OntoUML](https://matheuslenke.github.io/tonto-docs/) and their [Visual Paradigm plugin](https://github.com/OntoUML/ontouml-vp-plugin) --- example diagrams are available in the readme for the latter
* [Integrating Langium with a UI Editor like React-flow](https://github.com/eclipse-langium/langium/discussions/1373)
* [SvelteFlow](https://svelteflow.dev/)

#### Explainability

* [Whyline: "a debugging tool that allows programmers to ask "Why did" and "Why didn't" questions about their program's output".](https://www.cs.cmu.edu/~NatProg/whyline.html) Useful ideas here, both UX and implementation-wise, for both the 'chatobt' and IDE.

#### Visual diagramming languages

* <https://www.thestrangeloop.com/2022/diagrammar-simply-make-interactive-diagrams.html>
* An *un*related diagrammar, this time a dataflow PL: <https://github.com/billbudge/Diagrammar>
* See [Enso IDE](https://github.com/enso-org/enso/tree/develop/app/gui2) for a nice example of a visual programming language / editor
* Visual Haskell: <https://github.com/rgleichman/glance> and <https://github.com/Only1Loatheb/glance/> and <https://www.youtube.com/watch?v=cb25Ts4rLXA>
* [Flyde](https://github.com/flydelabs/flyde) "is an open-source visual programming language built to integrate with your existing codebase. It allows you to create and run visual programs and is designed to complement and enhance traditional textual coding, not to replace it."
* [Flowblocks](https://github.com/jyjblrd/flowblocks) "FlowBlocks is a visual interface for programming the Raspberry Pi Pico [...] With FlowBlocks, users can build programs by dragging and dropping blocks from a comprehensive standard library of inputs, operators, and outputs, and connecting them into flowcharts, which describe the flow of data throughout a program. In addition, experienced users also have the option of creating custom blocks—written with a templating dialect of MicroPython"
* [To Dissect a Mockingbird: A Graphical Notation for the Lambda Calculus with Animated Reduction](<https://dkeenan.com/Lambda/>)

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

### Deontic modality / expressing requirements

* [Knowledge-Assisted Reasoning of Model-Augmented System Requirements with Event Calculus and Goal-Directed Answer Set Programming](https://arxiv.org/abs/2109.04634)
* Normative requirements for regulatory compliance: An abstract formal framework

### On keeping the underlying language grammar relatively constrained, but still understandable to non-professional-programmers via UI/UX

* [The Enso Language](https://github.com/enso-org/enso), a language for data processing, seems to also [subscribe to a lot of the same tenets](https://github.com/enso-org/enso/blob/develop/docs/enso-philosophy.md), e.g.
  * "Explicit Over Implicit: The design of Enso and its libraries should *always* account for making all behaviour explicit."
  * "Unified Syntax: The language syntax should be simple, concise, and be usable on both the type and term levels.
  * "Choice in a language inherently creates myriad ways to do things. This is singularly unhelpful in any programming language, let alone one intended to span all experience levels, as it increases the cognitive burden of using the language."
