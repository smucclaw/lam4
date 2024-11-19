/*
Checking this in as something that might be useful down the road.
NOT using yet because some of the zod-to-json-schema looks a bit weird.

Also this won't work without refactoring visualizeProgramRequest.ts,
because of the vscode RequestType import. Will fix that later.
*/

import { z } from "zod";
import { zodToJsonSchema } from "zod-to-json-schema";
import {
  visualizeProgramResponseSchema,
  visualizeProgramInfoSchema,
  visualizeProgramErrorSchema,
} from "../src/interfaces";
import * as fs from "fs";

// TODO: Refactor to only have one source of truth for the AsyncResult types

const asyncResultSchema = z.discriminatedUnion("status", [
  // idle/loading
  z.object({
    status: z.enum(["idle", "loading"]),
    data: z.optional(visualizeProgramResponseSchema),
    error: z.optional(visualizeProgramErrorSchema),
  }),
  // success
  z.object({
    status: z.literal("success"),
    data: visualizeProgramResponseSchema,
    error: z.optional(visualizeProgramErrorSchema),
  }),
  // error
  z.object({
    status: z.literal("error"),
    error: visualizeProgramErrorSchema,
    data: z.optional(visualizeProgramResponseSchema),
  }),
]);

const visualizeProgramProtocolSchema = {
  $schema: "http://json-schema.org/draft-07/schema#",
  method: "l4/visualizeProgram",
  params: zodToJsonSchema(visualizeProgramInfoSchema),
  response: zodToJsonSchema(asyncResultSchema),
};

export function generateProtocolSchema(outputPath: string = "out/visualizeProgramProtocol.json"): void {
  const jsonSchema = JSON.stringify(visualizeProgramProtocolSchema, null, 2);
  console.log(jsonSchema);
  fs.writeFileSync(outputPath, jsonSchema);
}

if (require.main === module) {
  generateProtocolSchema();
}
