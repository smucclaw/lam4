import { DocumentState, URI, type TextDocument } from 'langium';
import { DefaultDocumentUpdateHandler } from 'langium/lsp';
import type { TextDocumentChangeEvent } from 'vscode-languageserver';
import type { LangiumSharedServices } from 'langium/lsp';
import { Logger } from "tslog";
import { exec } from 'child_process';
import 'dotenv/config';
import makeClient from '../remote-decision-service-api/api.js';
import { paths } from '../remote-decision-service-api/api.js';

/** Flag: Whether to update */
const UPDATE_BACKEND_ON_SAVE = process.env.UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS === "update";
// const MEDIA_TYPE = "application/json";
const routes = {"all_rules": "/functions/",
                "business_rules": "functions/business_rules",
                "eval": "functions/business_rules/evaluation"
              }


if (process.env.UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS === undefined) {
  throw new Error("UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS environment variable is not present in .env");
}
if (process.env.REMOTE_DECISION_SERVICE_URL === undefined) {
  throw new Error("REMOTE_DECISION_SERVICE_URL environment variable is not present in .env");
}


const client = makeClient(process.env.REMOTE_DECISION_SERVICE_URL);

const docUpdateLogger = new Logger({ 
  name: "docUpdate",
  minLevel: 1,
  prettyLogTemplate: "{{name}}  {{logLevelName}}  "});


export class Lam4DocumentUpdateHandler extends DefaultDocumentUpdateHandler {

  constructor(services: LangiumSharedServices) {
    super(services);
  }


  // override didSaveDocument?(event: TextDocumentChangeEvent<TextDocument>): void {
      
  //   // TODO: In the future, I want to have a daemon Lam4 HS-backend server and just send a request to it to compile things and get a Simala program back
    
  //   try {
  //     // 1. Cabal run script to generate Simala program

  //     const runLam4CLICmd = "cabal run lam4-cli --";
  //   // 2. Read in Simala program
  //   // 3. Send a PUT request to update the program on the server
  //   } catch (error) {
  //   }
  // }
}

// function serializeAndSaveToDisk() {
//   const program = await getProgramAst(fileName);
//   const noMetadataAstString = serializeProgramToJson(program, noMetadataSerializationConfig) as string;
//   const astJsonOutPath = writeToDisk(noMetadataAstString, fileName, 
//       "", opts.destination);

//   consoleLogAST(noMetadataAstString);


// }


async function getRulesOnRemoteDecisionService() {
  const { data, error } = await client.GET(routes["all_rules"] as any, {});
  return {data, error}
}
// async function updateRemoteDecisionServiceWithNewProgram() {
//   // TODO
//   // Send a POST request to update the program on the server
// }

//quick test
const obj = await getRulesOnRemoteDecisionService();
console.log(obj.data);