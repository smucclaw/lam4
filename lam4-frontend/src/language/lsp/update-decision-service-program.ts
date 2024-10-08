import { URI } from "langium";
import type { LangiumDocument, AstNode } from "langium";
import type { LangiumSharedServices } from "langium/lsp";
import {APIClient} from "../remote-decision-service-api/api.js";
import type {
  components,
} from "../remote-decision-service-api/api.js";
import { config } from "./config.js";
// import type { Connection } from 'vscode-languageserver';
import { DiagnosticSeverity } from "vscode-languageserver";
// import {UPDATE_REMOTE_DECISION_SERVICE_PROGRAM_REQUEST} from './messages.js';
import { Uri } from "vscode";
import path from "path";
import { execa } from "execa";
import fs from "fs-extra";

// -- TODO: These should be put in, and read from, the .env file
const OUTPUT_DIR = "generated/simala";
const DEFAULT_SIMALA_OUTPUT_PROGRAM_FILENAME = "output.simala";
const DEFAULT_OUTPUT_PROGRAM_INFO_FILENAME = "program_info.json";
const DEFAULT_ENDPOINT_NAME = "business_rules"; // aka DEFAULT_FUNCTION_NAME

const CMD_RUN_LAM4_CLI = "lam4-cli";
const DEFAULT_OUTPUT_DIR = "generated/";
const DEFAULT_OUTPUT_SIMALA_PROGRAM_PATH = path.join(DEFAULT_OUTPUT_DIR, DEFAULT_SIMALA_OUTPUT_PROGRAM_FILENAME);
const DEFAULT_OUTPUT_PROGRAM_INFO_PATH = path.join(OUTPUT_DIR, DEFAULT_OUTPUT_PROGRAM_INFO_FILENAME);
// In the long term, will probably have a daemon and communicate back and forth over rpc instead of via file-based IO


const getPayloadMakerArgs = (simalaProgramPath?: string) => [
  "create",
  "--mapping",
  config.getDataModelXmlPath(),
  "--simala",
  simalaProgramPath ?? DEFAULT_OUTPUT_SIMALA_PROGRAM_PATH,
];

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

const MEDIA_TYPE = "application/json";

/*********************
     Client, Logger
**********************/
const client = APIClient.make(config);
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
  if (document.diagnostics?.some((e) => e.severity === DiagnosticSeverity.Error)) {
    logger.error("Document has errors, cannot update remote decision service program till errors are fixed");
  } else {
    await unsafeUpdateDecisionServiceProgram(document);
  }
}

/**
 */
async function unsafeUpdateDecisionServiceProgram(programInfo: ProgramInfo) {
  const uri = programInfo.uri;
  // TODO: In the future, I want to have a daemon Lam4 HS-backend server and just send a request to it to compile things and get a Simala program back
  logger.info("unsafeUpdateDecisionServiceProgram called with ", uri.fsPath);

  // const basefilename = path.parse(uri.fsPath).name;
  const compiledOutputInfo: CompiledOutputInfo = {
    simalaProgramPath: DEFAULT_OUTPUT_SIMALA_PROGRAM_PATH,
    programInfoPath: DEFAULT_OUTPUT_PROGRAM_INFO_PATH,
  };

  // TODO2: Don't do the frontend parsing twice
  await compileToSimala(uri);

  const stockProgramPayload = await runPayloadMakerWithCompiledOutput(compiledOutputInfo);
  // logger.debug(JSON.stringify(stockProgramPayload));

  const updatedProgramPayload = await updatePayloadWithProgramInfo(compiledOutputInfo, stockProgramPayload);
  await fs.writeJSON("./generated/tmp/programPayload.json", updatedProgramPayload);

  const endpointName = updatedProgramPayload.declaration?.function?.name ?? DEFAULT_ENDPOINT_NAME;


  await client.updateWithProgram(endpointName, "/functions/{name}", {
    params: { path: { name: endpointName } },
    body: updatedProgramPayload,
    headers: {"Content-Type": MEDIA_TYPE}
  });

}


async function compileToSimala(uri: Uri) {
  try {
    const { stdout } = await execa(CMD_RUN_LAM4_CLI, [uri.fsPath]);
    logger.info("Transpiled Lam4 to Simala. Short excerpt of stdout:\n", stdout.substring(0, 200)); //temp, for prototyping
  } catch (error) {
    logger.error("Error when transpiling Lam4 to Simala: ", error);
  }
}

async function updatePayloadWithProgramInfo(
  outputProgramInfo: CompiledOutputInfo,
  payload: DecisionServiceFunctionDeclPayload,
) {
  const pload: DecisionServiceFunctionDeclPayload = { ...payload };
  const programInfo = await fs.readJSON(outputProgramInfo.programInfoPath);
  const program = await fs.readFile(outputProgramInfo.simalaProgramPath, "utf8");

  // [TODO/TO-REFACTOR] getting the first entrypoint function name for demo
  if (pload.declaration?.function?.name && pload.implementation) {
    pload.declaration.function.name = programInfo.entryPointFunctions[0].functionName.textName;
    pload.implementation = [["simala", program]];

    // logger.debug("pload is\n", pload);

    return pload;
  } else {
    throw new Error("Error when updating payload with program info");
  }
}

async function runPayloadMakerWithCompiledOutput(outputProgramInfo: CompiledOutputInfo) {
  const payloadMakerArgs = getPayloadMakerArgs(outputProgramInfo.simalaProgramPath);
  logger.debug("payloadMakerArgs", payloadMakerArgs);
  const { stdout, stderr } = await execa(config.getUploadProgramPayloadMakerCmd(), payloadMakerArgs);
  const payload = JSON.parse(stdout);

  if (stdout) {
    return payload;
  } else {
    logger.error("Error when running payload maker: ", stderr);
  }
}

// function serializeAndSaveToDisk() {
//   const program = await getProgramAst(fileName);
//   const noMetadataAstString = serializeProgramToJson(program, noMetadataSerializationConfig) as string;
//   const astJsonOutPath = writeToDisk(noMetadataAstString, fileName,
//       "", opts.destination);

//   consoleLogAST(noMetadataAstString);

// }
