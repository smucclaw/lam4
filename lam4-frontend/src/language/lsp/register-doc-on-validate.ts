// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck
import { DocumentState, 
  // URI, 
  // type TextDocument 
} from 'langium';

import type { 
  LangiumSharedServices } from 'langium/lsp';

// import {config} from "./config.js";
import type { Connection } from 'vscode-languageserver';
import { DOCUMENTS_VALIDATED_NOTIFICATION } from './messages.js';

// const logger = config.getLogger();

export function registerOnDocValidateHandler(connection: Connection, sharedServices: LangiumSharedServices): void {
  const documentBuilder = sharedServices.workspace.DocumentBuilder;
  // const documents = sharedServices.workspace.LangiumDocuments;
  documentBuilder.onBuildPhase(DocumentState.Validated, documents => {
      // const docPaths = documents.map(doc => doc.uri.path);
      // logger.debug("validated docs paths ", docPaths);

      const uris = documents.map(e => e.uri.toString());

      // For webviews / other consumers that need notifications on the language client
      connection.sendNotification(DOCUMENTS_VALIDATED_NOTIFICATION, uris);
  });
}