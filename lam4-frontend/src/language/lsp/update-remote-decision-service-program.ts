import { 
  URI, 
} from 'langium';
// import type { TextDocumentChangeEvent } from 'vscode-languageserver';
import type { LangiumSharedServices } from 'langium/lsp';
// import makeClient from '../remote-decision-service-api/api.js';
// import { paths } from '../remote-decision-service-api/api.js';
import {config} from "./config.js";
import { exec } from 'child_process';
// import type { Connection } from 'vscode-languageserver';
import { DiagnosticSeverity } from 'vscode-languageserver';
// import {UPDATE_REMOTE_DECISION_SERVICE_PROGRAM_REQUEST} from './messages.js';
import { Uri } from 'vscode';

const CMD_RUN_LAM4_CLI = "cabal run lam4-cli -- ";

// type DocumentUri = string;
// const MEDIA_TYPE = "application/json";
// const routes = {"all_rules": "/functions/",
//                 "business_rules": "functions/business_rules",
//                 "eval": "functions/business_rules/evaluation"
//               }

/*********************
     Client, Logger 
**********************/
// const client = makeClient(config);
const logger = config.getLogger();

// TODO: Try adding a cmd handler in the future
// export function registerUpdateRemoteDecisionServiceProgramHandler(connection: Connection, services: LangiumSharedServices): void {
//   connection.onRequest(UPDATE_REMOTE_DECISION_SERVICE_PROGRAM_REQUEST,
//      async (uri: DocumentUri) => {
//       const parsedUri = URI.parse(uri);
//       await updateRemoteDecisionServiceProgramViaCLI(services, parsedUri);
//     });
// }

export async function updateRemoteDecisionServiceProgramViaCLI(services: LangiumSharedServices, uri: URI) {
  // Check if doc is validated first

  // const documentBuilder = services.workspace.DocumentBuilder;
  const documents = services.workspace.LangiumDocuments;
  const document = await documents.getOrCreateDocument(uri);
  if (document.diagnostics?.some(e => e.severity === DiagnosticSeverity.Error)) {
    logger.error("Document has errors, cannot update remote decision service program till errors are fixed");
  } else {
    await unsafeUpdateRemoteDecisionServiceProgram(uri); 
  }
}

/** 
 * Examples of API calls:
 *  https://github.com/smucclaw/royalflush/tree/fendor/oia-to-simala/oia-rules/web-service
 * https://github.com/smucclaw/royalflush/blob/fendor/oia-to-simala/oia-rules/web-service/short_examples.md
 */
async function unsafeUpdateRemoteDecisionServiceProgram(uri: Uri) {
  // TODO: In the future, I want to have a daemon Lam4 HS-backend server and just send a request to it to compile things and get a Simala program back
  logger.info("unsafeUpdateRemoteDecisionServiceProgram called with ", uri.fsPath);

  try {
    runLam4CLI(uri);
  } catch (error) {
    logger.error("Error when updating remote decision service program: ", error);
  }
}

export function runLam4CLI(uri: Uri) {
  const runLam4CLI = CMD_RUN_LAM4_CLI + uri.fsPath;
  exec(runLam4CLI, (error, stdout) => {
    if (error) {
      console.error("Error when transpiling Lam4 to Simala: ", error);
    } else if (stdout) {
      logger.info("Transpiled Lam4 to Simala: ", stdout); //temp, for prototyping
    }
  });
}
// function serializeAndSaveToDisk() {
//   const program = await getProgramAst(fileName);
//   const noMetadataAstString = serializeProgramToJson(program, noMetadataSerializationConfig) as string;
//   const astJsonOutPath = writeToDisk(noMetadataAstString, fileName, 
//       "", opts.destination);

//   consoleLogAST(noMetadataAstString);


// }


// async function getRulesOnRemoteDecisionService() {
//   const { data, error } = await client.GET(routes["all_rules"] as any, {});
//   return {data, error}
// }
// async function updateRemoteDecisionServiceWithNewProgram() {
//   // TODO
//   // Send a POST request to update the program on the server
// }

//quick test
// const obj = await getRulesOnRemoteDecisionService();
// console.log(obj.data);


// const hi = await client.GET("/functions/business_rules" as any, {});
