import { startLanguageServer } from 'langium/lsp';
import { NodeFileSystem } from 'langium/node';
import { createConnection, ProposedFeatures } from 'vscode-languageserver/node.js';
import { createLam4Services } from './lam4-module.js';
import { registerOnDocValidateHandler } from './lsp/register-doc-on-validate.js';
// import {registerUpdateRemoteDecisionServiceProgramHandler} from './lsp/update-remote-decision-service-program.js';

// Create a connection to the client
const connection = createConnection(ProposedFeatures.all);

// Inject the shared services and language-specific services
const { shared } = createLam4Services({ connection, ...NodeFileSystem });

registerOnDocValidateHandler(connection, shared);
// TODO: maybe, for future
// registerUpdateProgramOnBackendHandler(connection);
// registerUpdateRemoteDecisionServiceProgramHandler(connection, shared);

// Start the language server with the shared services
startLanguageServer(shared);
