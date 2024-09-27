import { z } from 'zod';
import 'dotenv/config';
import { Logger } from "tslog";

type BaseConfig = z.infer<typeof configSchema>;
export type Config = BaseConfig & {logger: Logger<unknown>};

/*****************
  LSP Logger 
******************/
const lspLogger = new Logger({ 
  name: "LSP",
  minLevel: 1,
  prettyLogTemplate: "{{name}}  {{logLevelName}}  "});

/*****************
  .env config 
******************/
const configSchema = z.object({
  REMOTE_DECISION_SERVICE_URL: z.string(),
  UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS: z.enum(['update', 'no_update']),
});

const baseConfig = configSchema.parse(process.env);
export const config = {...baseConfig, logger: lspLogger};

console.log('Env:', baseConfig);