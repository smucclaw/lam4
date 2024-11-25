# README

## General resources for visualizer interfaces

Similar things that other people have done

* ProofWidgets, a library of user interface components for Lean 4
  * [Github codebase](https://github.com/leanprover-community/ProofWidgets4)
  * [Paper: An Extensible User Interface for Lean 4](https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ITP.2023.24)
  
* [Lean 4 VSCode extension](https://github.com/leanprover/vscode-lean4/)

* [Sterling: A Web-based Visualizer for Relational Modeling Languages](https://sterling-js.github.io/) -- useful inspiration for interfaces for automata visualizer as well as potentially visualizers of datatypes in the future
  * [The paper](https://jwbaugh.github.io/papers/dyer-abz-2021.pdf)
  * Codebases
    * [Web browser side of Sterling. It includes an API used to represent Alloy instances in JS as well as the user interface and visualization code.](https://github.com/atdyer/sterling)
    * [The custom Alloy build that comes packaged with the Sterling interface. It includes minor updates to the Alloy user interface as well as the server code used to communicate with the browser.](https://github.com/alloy-js/sterling)
    * [https://github.com/alloy-js/sterling-js](https://github.com/alloy-js/sterling-js)
    * [https://github.com/sterling-ts/sterling-ts](https://github.com/sterling-ts/sterling-ts)
  * See also [this paper](https://cs.brown.edu/~tbn/publications/forge-oopsla24.pdf) for more nice examples of domain-specific visualizations

## L4-specific resources re visualizations

* [Internal docs page on L4 visualizer components, with links to their (software design) interfaces](https://github.com/smucclaw/internal_docs/blob/main/docs/current_system/codebase/visualizations.md)

## Other useful resources on language server RPC things

[https://github.com/dotnet/vscode-csharp/tree/main/src/razor/src/rpc](https://github.com/dotnet/vscode-csharp/tree/main/src/razor/src/rpc)
though they do have a lot of functionality that we probably don't need

The things we might need to add in the medium term might be things like the following (or similar things in the visualization sphere):

* https://github.com/dotnet/vscode-csharp/blob/main/src/razor/src/rpc/serializableRange.ts
* https://github.com/dotnet/vscode-csharp/blob/main/src/razor/src/rpc/razorMapToDocumentRangesRequest.ts
* https://github.com/dotnet/vscode-csharp/blob/main/src/razor/src/rpc/razorMapToDocumentRangesResponse.ts

* https://github.com/dotnet/vscode-csharp/blob/main/src/razor/src/rpc/serializableTextEdit.ts
* https://github.com/dotnet/vscode-csharp/blob/main/src/razor/src/rpc/serverTextSpan.ts
* https://github.com/dotnet/vscode-csharp/blob/main/src/razor/src/rpc/serverTextChange.ts

* [Semantic tokens](https://github.com/dotnet/vscode-csharp/tree/5b414fa5413c85641bb07c4bb4116347dda9a4a9/src/razor/src/semantic) --- though Hannes probably already has this sorted out.

Most of these seem like they'd be straightforward to add later, so I'll probably just wait till it's clear they are needed before adding them.