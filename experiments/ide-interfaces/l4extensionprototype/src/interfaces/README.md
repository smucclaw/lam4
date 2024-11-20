# README

## Useful resources

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