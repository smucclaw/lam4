import { URI } from 'vscode-uri';
import { RequestType } from "vscode-languageclient/node";
import { z } from "zod";

/*****************
  Util functions
*******************/

const uriSchema = z
  .string()
  .transform((str) => URI.parse(str))
  .or(z.custom<URI>(str => URI.isUri(str)))
  .transform((uri) => uri.toString());
  
/*******************************
     VisualizeProgramRequest
********************************/

/**
 * Request type for visualizing L4 programs.
 * Can think in the future about whether to have different requests for visualizations of different L4 constructs.
 *
 * @type {RequestType<VisualizeProgramInfo, AsyncResult<VisualizeProgramResponse>, any>}
 *
 * @remarks
 * What RequestType's type parameters mean (yes, `RequestType` is a confusing type):
 * - Params: VisualizeProgramInfo
 * - Result: AsyncResult<VisualizeProgramResponse, VisualizeProgramError>
 * - Error: any (I don't currently understand what this type parameter does, but it shouldn't matter for our purposes.)
 */
export namespace VisualizeProgramRequest {
  export const type: RequestType<
    VisualizeProgramInfo,
    AsyncResult<VisualizeProgramResponse, VisualizeProgramError>,
    any
  > = new RequestType("l4/visualizeProgram");
}

/** This is basically
    ```
      type VisualizeProgramInfo = {
          program: URI;
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


/******************
     Util types
*******************/

// From https://github.com/gvergnaud/ts-pattern/blob/b5aa458c031c08150689be8c4a2d0dc0c2b10093/tests/types-catalog/utils.ts#L78

type AsyncResultStatus = "idle" | "loading" | "error" | "success";

export interface BaseAsyncResult<TData, TError = Error> {
  status: AsyncResultStatus;
  data?: TData;
  error?: TError;
}

export interface AsyncResultIdleOrLoading<TData, TError = Error> extends BaseAsyncResult<TData, TError> {
  status: "idle" | "loading";
}

export interface AsyncResultSuccess<TData, TError = Error> extends BaseAsyncResult<TData, TError> {
  status: "success";
  data: TData;
}

export interface AsyncResultError<TData, TError = Error> extends BaseAsyncResult<TData, TError> {
  status: "error";
  error: TError;
}
export type AsyncResult<TData, TError = Error> =
  | AsyncResultIdleOrLoading<TData, TError>
  | AsyncResultSuccess<TData, TError>
  | AsyncResultError<TData, TError>;
