import { z } from 'zod';
import 'dotenv/config';

const configSchema = z.object({
  REMOTE_DECISION_SERVICE_URL: z.string(),
  UPDATE_REMOTE_DECISION_SERVICE_ON_SAVE_STATUS: z.enum(['update', 'no_update']),
});
export type Config = z.infer<typeof configSchema>;

export const config = configSchema.parse(process.env);
console.log('Env:', config);