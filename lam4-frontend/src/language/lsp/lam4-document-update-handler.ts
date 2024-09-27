import { 
  URI, 
  type TextDocument } from 'langium';
import { DefaultDocumentUpdateHandler } from 'langium/lsp';
import type { TextDocumentChangeEvent } from 'vscode-languageserver';
import type { LangiumSharedServices } from 'langium/lsp';
import {config} from "./config.js";
// import { Utils } from 'vscode-uri';
import {updateRemoteDecisionServiceProgramViaCLI} from './update-remote-decision-service-program.js';

/*********************
     Logger 
**********************/
const logger = config.getLogger();

/****************************
  Lam4DocumentUpdateHandler 
*****************************/
export class Lam4DocumentUpdateHandler extends DefaultDocumentUpdateHandler {
  #sharedServices: LangiumSharedServices;
  constructor(services: LangiumSharedServices) {
    super(services);
    this.#sharedServices = services;
  }

  // Go down this path only if onSave doesn't feel snappy enough
  // override didChangeContent(change: TextDocumentChangeEvent<TextDocument>): void {
  //   super.didChangeContent(change);
  //   const documentUri = change.document.uri;
  //   config.updateDocChangedTimestamp(documentUri);
  // }

  didSaveDocument(event: TextDocumentChangeEvent<TextDocument>): void {
    const uri = URI.parse(event.document.uri);
    logger.debug("didSaveDocument called! ", uri.fsPath);

    updateRemoteDecisionServiceProgramViaCLI(this.#sharedServices, uri);
  }
}

