import { URI } from "vscode-uri";
import { RequestType } from "vscode-languageclient/node";
import { z } from "zod";

/*****************
  Util functions
*******************/

export const uriSchema = z
  .string()
  .transform((str) => URI.parse(str))
  .or(z.custom<URI>((str) => URI.isUri(str)))
  .transform((uri) => uri.toString());

/*******************************
     VisualizeProgramRequest
********************************/

/**
 * Request type for visualizing L4 programs.
 * Can think in the future about whether to have different requests for visualizations of different L4 constructs.
 *
 * @type {RequestType<VisualizeProgramInfo, VisualizeProgramResponse, void>}
 *
 * @remarks
 * What RequestType's type parameters mean (yes, `RequestType` is a confusing type):
 * - Params: VisualizeProgramInfo
 * - Result: VisualizeProgramResponse
 * - Error: void (I don't currently understand what this type parameter does, but it shouldn't matter for our purposes.)
 *
 * Method Name: `"l4/visualizeProgram"`
 */
export namespace VisualizeProgramRequest {
  export const type: RequestType<
    VisualizeProgramInfo,
    VisualizeProgramResponse,
    void
  > = new RequestType("l4/visualizeProgram");
}

/* aka:

type VisualizeProgramRequestSchema {
  method: "l4/visualizeProgram";
  params: VisualizeProgramInfo;
  result: VisualizeProgramResponse;
}

*/

/** This is basically
    ```
      type VisualizeProgramInfo = {
          program: URI; // which in json land is just `string`
      }
    ```
*/
export const visualizeProgramInfoSchema = z.object({
  program: uriSchema,
});

export const visualizeProgramResponseSchema = z.object({
  html: z.string(),
});

export const visualizeProgramErrorSchema = z.object({
  errorMessage: z.string(),
});

export type VisualizeProgramInfo = z.infer<typeof visualizeProgramInfoSchema>;
export type VisualizeProgramResponse = z.infer<typeof visualizeProgramResponseSchema>;
export type VisualizeProgramError = z.infer<typeof visualizeProgramErrorSchema>;
