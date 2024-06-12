import { startLanguageServer } from 'langium/lsp';
import { NodeFileSystem } from 'langium/node';
import { createConnection, ProposedFeatures } from 'vscode-languageserver/node.js';
import { createMiniL4TypesServices } from './mini-l4-types-module.js';

// Create a connection to the client
const connection = createConnection(ProposedFeatures.all);

// Inject the shared services and language-specific services
const { shared } = createMiniL4TypesServices({ connection, ...NodeFileSystem });

// Start the language server with the shared services
startLanguageServer(shared);
