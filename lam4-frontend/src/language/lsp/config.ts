import { z } from 'zod';
import 'dotenv/config';
import * as fs from 'fs';
import { Logger } from "tslog";
import { getCurrentTimestamp } from '../../utils.js';

type BaseConfig = z.infer<typeof configSchema>;

/*****************
  LSP Logger 
******************/
const lspLogger = new Logger({ 
  name: "LSP",
  // minLevel: 1,
  prettyLogTemplate: "{{name}}  {{logLevelName}}  "});

export type LspLogger = typeof lspLogger;


/*****************
  .env config 
******************/
const configSchema = z.object({
  REMOTE_DECISION_SERVICE_URL: z.string(),
  UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS: z.enum(['update', 'no_update']),
  DECISION_SERVICE_REQUEST_MAKER_CMD: z.string(),
  DEMO_OIA_DATA_MODEL_XML_PATH: z.string()
});


export class LSPConfig {
  #logger: typeof lspLogger;
  #docLastChangedTimestamp: Map<string, number>;
  #docTranspiledTimestamp: Map<string, number>;

  #decisionServiceUrl: string;
  #updateDecisionServiceOnSaveStatus: 'update' | 'no_update';
  #decisionServiceRequestMakerCmd: string;
  #oiaDataModelPath: string;

  constructor(baseConfig: BaseConfig, logger: typeof lspLogger) {
    this.#decisionServiceUrl = baseConfig.REMOTE_DECISION_SERVICE_URL;
    this.#updateDecisionServiceOnSaveStatus = baseConfig.UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS;
    this.#decisionServiceRequestMakerCmd = baseConfig.DECISION_SERVICE_REQUEST_MAKER_CMD;
    this.#oiaDataModelPath = baseConfig.DEMO_OIA_DATA_MODEL_XML_PATH;

    this.#logger = logger;

    this.#docLastChangedTimestamp = new Map();
    this.#docTranspiledTimestamp = new Map();
  }

  // ----- Getters for constants ---------- //

  getDecisionServiceUrl() {
    return this.#decisionServiceUrl;
  }

  getUpdateRemoteDecisionServiceOnSave() {
    return this.#updateDecisionServiceOnSaveStatus;
  }

  getUploadProgramPayloadMakerCmd() {
    return this.#decisionServiceRequestMakerCmd;
  }

  getDataModelXmlPath() {
    return this.#oiaDataModelPath;
  }
  
  /** Logger */
  getLogger() {
    return this.#logger;
  }

  updateDocChangedTimestamp(stringifiedURI: string) {
    this.#docLastChangedTimestamp.set(stringifiedURI, getCurrentTimestamp());
  }
  getDocChangedTimestamp(stringifiedURI: string) {
    return this.#docLastChangedTimestamp.get(stringifiedURI);
  }

  updateDocTranspiledTimestamp(stringifiedURI: string) {
    this.#docTranspiledTimestamp.set(stringifiedURI, getCurrentTimestamp());
  }

  isTranspiledLam4UpToDate(stringifiedURI: string) {
    const [docLastChangedTimestamp, docTranspiledTimestamp] = [ this.#docLastChangedTimestamp.get(stringifiedURI), 
                                                                this.#docTranspiledTimestamp.get(stringifiedURI) ];
    if (!docLastChangedTimestamp || !docTranspiledTimestamp) {
      return false;
    } else {
      return docLastChangedTimestamp < docTranspiledTimestamp; 
    }
  }
}

const baseConfig = configSchema.parse(process.env);

// Other quick checks of the config / env
if (!fs.existsSync(baseConfig.DEMO_OIA_DATA_MODEL_XML_PATH)) {
  lspLogger.error(`The DEMO_OIA_DATA_MODEL_XML_PATH in the .env does NOT exist: ${baseConfig.DEMO_OIA_DATA_MODEL_XML_PATH}`);
}

export const config = new LSPConfig(baseConfig, lspLogger);

lspLogger.debug('Env:', baseConfig);