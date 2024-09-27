import { z } from 'zod';
import 'dotenv/config';
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

/*****************
  .env config 
******************/
const configSchema = z.object({
  REMOTE_DECISION_SERVICE_URL: z.string(),
  UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS: z.enum(['update', 'no_update']),
});


export class LSPConfig {
  #logger: typeof lspLogger;
  #docLastChangedTimestamp: Map<string, number>;
  #docTranspiledTimestamp: Map<string, number>;

  #remote_decision_service_url: string;
  #update_remote_decision_service_on_save_status: 'update' | 'no_update';

  constructor(baseConfig: BaseConfig, logger: typeof lspLogger) {
    this.#remote_decision_service_url = baseConfig.REMOTE_DECISION_SERVICE_URL;
    this.#update_remote_decision_service_on_save_status = baseConfig.UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS;

    this.#logger = logger;

    this.#docLastChangedTimestamp = new Map();
    this.#docTranspiledTimestamp = new Map();
  }
  get_remote_decision_service_url() {
    return this.#remote_decision_service_url;
  }

  getUpdateRemoteDecisionServiceOnSave() {
    return this.#update_remote_decision_service_on_save_status;
  }

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
export const config = new LSPConfig(baseConfig, lspLogger);

lspLogger.debug('Env:', baseConfig);