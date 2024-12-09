# Nov 18 2024: WIP proposal for L4 IDE LSP things, with list of things I'm not sure of

## My rough plan

1. Sketch rough proposal and questions / things we need to investigate more
2. After clarifying rough proposal: Start building / sketching prototype of interfaces (don't need to implement everything --- focus would be on the types / interfaces)

## What seems uncontroversial

### Uncontroversial parts of the high level architecture

The system will include at least the following

1. Language server
2. A VSCode extension with a webview
3. For every visualization or service that should be provided by something other than the compiler, there should be something that is responsible for that

Services like visualizing some bit of decision logic can be triggered with, e.g., VSCode commands (or buttons somewhere or whenever the user saves the file or ...) The VSCode extension would be responsible for registering these commands.

When we want to, e.g., visualize some bit of decision logic, the extension will fire off a request to the language server (via `sendRequest`). This request will have a pre-specified type that both the extension and language server will be aware of. The language server will, upon receiving this request, respond to the extension with something that the extension will be able to use to render html in the webview (I am deliberately not committing to specifics about what exactly should be returned in the response --- see the following section).

For examples of `sendRequest` / `onRequest` in the wild, see
* [https://github.com/microsoft/vscode-extension-samples/blob/9378d7c9299e543713a9eae22eadda70b7fc9431/wasm-language-server/client/src/extension.ts#L59](https://github.com/microsoft/vscode-extension-samples/blob/9378d7c9299e543713a9eae22eadda70b7fc9431/wasm-language-server/client/src/extension.ts#L59)
* TypeFox's railroad diagram webview and handler on the LSP
  * https://github.com/eclipse-langium/langium/blob/644660a14148787f428ab440e53ea7821c7c9ff7/packages/langium-vscode/src/railroad-webview.ts
  * https://github.com/eclipse-langium/langium/blob/644660a14148787f428ab440e53ea7821c7c9ff7/packages/langium-vscode/src/language-server/railroad-handler.ts


## Things to iron out

### Meta-question: What is the bare minimum we need to figure out before we can start parallelizing work more?

### What exactly should the language server return for something like visualizing decision logic? Where should the abstraction boundary be?

There's a spectrum of options re how much consumers should know.

1. The extension / consumer of language services is the thinnest possible client

* The language server in effect returns HTML that the VSCode extension / webview can just render. 
  
* (The language server would presumably interact with some other part of the system to get that HTML; but how it does that is not something the VSCode extension would need to care about.)

* This option is still compatible with there being some kind of intermediate representation that the language server uses to interact with the service that does the visualizing. The key difference is maybe just that that would be a detail internal to the language service / its ecosystem --- rendering consumers like the extension / webview wouldn't need to know about this intermediate representation, or how the intermediate representation gets visualized.
  
2. There is some sort of intermediate representation that the extension gets from the language server, an intermediate representation that isn't HTML or any other directly-renderable format, but that's still much simpler than the actual abstract or concrete syntax of the language.

* If we go with this option, we'll want to make sure the intermediate representation includes whatever annotations are required for visualizing the decision logic in a readable way. E.g., this might include whatever NLG is required.

* (At the risk of saying the obvious, the intermediate representation should be structured: downstream consumers shouldn't have to parse bits of unstructured text.)

3. The language server just returns something very close to the AST/CST; i.e., the request would be more like a request for the AST/CST, as opposed to a request specifically for something decision-logic-related. 

Then again, maybe there isn't much of a practical difference between 1 and 2 in our context.

### What I'm currently leaning towards

In addition to the language server and extension, we will have something that can loosely speaking be thought of as having this type:

`L4Visualizer :: IR -> HTMLElement`

where `IR` is an intermediate representation that (i) is simpler than the actual L4 AST, but that (ii) still have regimented constructs for things like anaphora --- i.e., is still about the key domain concepts as opposed to 'vertical vs horizontal stacking of boxes'. And of course (iii) it should have annotations for things like whatever NLG we need. And (iv) it should be something that is in principle reasonably easy for other legal DSLs to compile to.

I'm calling it `L4Visualizer` instead of, e.g., `DecisionLogicVisualizer`, because I want to remain netural for now on whether we should have different visualizations for the decision logic vs. the deontic modals, or a component that somehow visualizes both at once.

The extension / webview is the thinnest possible client:

* when we want decision logic visualizations, the extension will ask the language server for that (so, i.e., we'd have a `RequestType` like `GetDecisionLogicVisualization`), and the extension will in effect get html back (or html wrapped in a record --- you get the idea). 

* The key thing is that the extension would not know anything about the IR.

## To note

### If what is wanted is something that can simulate ellipsis / anaphora, something like

> (1)   A person commits an offence, and is liable to banishment, if in a public place the person exhibits –

> (a)   a grumpy head or an unkind heart; and

> (b)   a bloody fist or a venomous claw or tooth.

(this is an example from one of the emails)

then the representation is returned from the language service should have a way of representing this sort of thing.

## Building for the web

Nothing concrete here; just some links to some resources.

https://code.visualstudio.com/api/extension-guides/web-extensions

* Limitations of web extensions, compared to normal extensions: https://code.visualstudio.com/api/extension-guides/web-extensions#web-extension-main-file
* LSP in web extensions: https://code.visualstudio.com/api/extension-guides/web-extensions#language-server-protocol-in-web-extensions
* https://github.com/microsoft/language-server-protocol/issues/528
  * especially https://github.com/microsoft/language-server-protocol/issues/528#issuecomment-406787716 though not sure if this is up to date
* https://www.hiro.so/blog/write-clarity-smart-contracts-with-zero-installations-how-we-built-an-in-browser-language-server-using-wasm
  * Looks like we would have to implement "a way for the server to call the VFS [exposed by VSCode] through requests to the client"

## Other resources or tools

https://github.com/smucclaw/internal_docs/blob/main/docs/current_system/codebase/visualizations.md

* https://github.com/TypeFox/vscode-messenger
* https://github.com/TypeFox/vscode-messenger/tree/main/packages/vscode-messenger-devtools

