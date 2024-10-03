import { 
  URI, 
} from 'langium';
import type {LangiumDocument, AstNode} from 'langium';
// import type { TextDocumentChangeEvent } from 'vscode-languageserver';
import type { LangiumSharedServices } from 'langium/lsp';
// import makeClient from '../remote-decision-service-api/api.js';
import type { 
              // paths, 
              components } from '../remote-decision-service-api/api.js';
import {config} from "./config.js";
// import { execFile } from 'child_process';
// import type { Connection } from 'vscode-languageserver';
import { DiagnosticSeverity } from 'vscode-languageserver';
// import {UPDATE_REMOTE_DECISION_SERVICE_PROGRAM_REQUEST} from './messages.js';
import { Uri } from 'vscode';
import path from 'path';
import {execa} from 'execa';

// -- TODO: These should be put in, and read from, the .env file
const OUTPUT_DIR = "generated/simala";
const DEFAULT_SIMALA_OUTPUT_PROGRAM_FILENAME = "output.simala";
const DEFAULT_OUTPUT_PROGRAM_INFO_FILENAME = "program_info.json";

const CMD_RUN_LAM4_CLI = "lam4-cli";
const DEFAULT_OUTPUT_DIR = "generated/"
const DEFAULT_OUTPUT_SIMALA_PROGRAM_PATH = path.join(DEFAULT_OUTPUT_DIR, DEFAULT_SIMALA_OUTPUT_PROGRAM_FILENAME); 
const DEFAULT_OUTPUT_PROGRAM_INFO_PATH = path.join(OUTPUT_DIR, DEFAULT_OUTPUT_PROGRAM_INFO_FILENAME);
// In the long term, will probably have a daemon and communicate back and forth over rpc instead of via file-based IO

const getPayloadMakerArgs = (simalaProgramPath?: string) => 
  ["create", 
    "--mapping", config.getDataModelXmlPath(), 
    "--simala", simalaProgramPath ?? DEFAULT_OUTPUT_SIMALA_PROGRAM_PATH];

// class UploadProgramRequestMaker {
//   // #template: UploadProgramPayload
  
//   static init() {

//   }

//   private constructor() {

//   }

//   makeUploadProgramRequest(outputInfo: CompiledOutputInfo) {
//     // Read in entrypoint info
//     // Tweak template with program and entrypoint info
//   } 
  
// }

type ProgramInfo = LangiumDocument<AstNode>;

interface CompiledOutputInfo {
  simalaProgramPath: string;
  programInfoPath: string;
}

/****************************************
    DecisionServiceFunctionDeclPayload
*****************************************/

type DecisionServiceFunctionDeclPayload = components["schemas"]["Implementation"];

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

export async function updateDecisionServiceProgramViaCLI(services: LangiumSharedServices, uri: URI) {
  // Check if doc is validated first

  // const documentBuilder = services.workspace.DocumentBuilder;
  const documents = services.workspace.LangiumDocuments;
  const document = await documents.getOrCreateDocument(uri);
  if (document.diagnostics?.some(e => e.severity === DiagnosticSeverity.Error)) {
    logger.error("Document has errors, cannot update remote decision service program till errors are fixed");
  } else {
    await unsafeUpdateDecisionServiceProgram(document); 
  }
}


/** 
 * Examples of API calls:
 *  https://github.com/smucclaw/royalflush/tree/fendor/oia-to-simala/oia-rules/web-service
 * https://github.com/smucclaw/royalflush/blob/fendor/oia-to-simala/oia-rules/web-service/short_examples.md
 */
async function unsafeUpdateDecisionServiceProgram(programInfo: ProgramInfo) {
  const uri = programInfo.uri;
  // TODO: In the future, I want to have a daemon Lam4 HS-backend server and just send a request to it to compile things and get a Simala program back
  logger.info("unsafeUpdateDecisionServiceProgram called with ", uri.fsPath);

  // const basefilename = path.parse(uri.fsPath).name;
  const compiledOutputInfo: CompiledOutputInfo = {
    simalaProgramPath: DEFAULT_OUTPUT_SIMALA_PROGRAM_PATH,
    programInfoPath: DEFAULT_OUTPUT_PROGRAM_INFO_PATH
  }

  let programPayload;
  try {
    // TODO: Need to tweak lam4-cli to also export the entrypoint info
    // TODO2: Don't do the frontend parsing twice
    await compileToSimala(uri); 
    programPayload = await runPayloadMakerWithCompiledOutput(compiledOutputInfo);
    logger.debug(JSON.stringify(programPayload));

    // makeRequest with payload
    // sendRequest
  } catch (error) {
    logger.error("Error when updating decision service program: ", error);
  }
}

async function compileToSimala(uri: Uri) {
  try {
    const { stdout } = await execa(CMD_RUN_LAM4_CLI, [uri.fsPath]);
    logger.info("Transpiled Lam4 to Simala. Short excerpt of stdout:\n", stdout.substring(0, 200)); //temp, for prototyping
  } catch (error) {
    logger.error("Error when transpiling Lam4 to Simala: ", error);
  }
}


// async function updateStockPayloadWithProgramInfo(payload: DecisionServiceFunctionDeclPayload) {

// }

async function runPayloadMakerWithCompiledOutput(outputProgramInfo: CompiledOutputInfo): Promise<DecisionServiceFunctionDeclPayload | undefined> {
  const requestMakerArgs = getPayloadMakerArgs(outputProgramInfo.simalaProgramPath);
  logger.debug("requestMakerArgs", requestMakerArgs);
  try {
    const { stdout } = await execa(config.getUploadProgramPayloadMakerCmd(), requestMakerArgs);
    const payload = JSON.parse(stdout);
    return payload;
  } catch (error) {
    logger.error("Error when running payload maker: ", error);
    return undefined;
  }
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
