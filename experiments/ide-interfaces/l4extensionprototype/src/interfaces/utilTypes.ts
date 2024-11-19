/*********************************
     Util types and Zod schemas
**********************************/

export interface JsonRPCMessage {
  readonly jsonrpc: "2.0";
  readonly id: number;
}

export interface JsonRPCRequest extends JsonRPCMessage {
  readonly method: string;
  readonly params: object | Array<any>;
}

/********************
  (Simple) Result
**********************/

export type Ok<T> = { type: "ok"; data: T };
export type ResError<T> = { type: "error"; error: T };

export type Result<TError, TOk> = Ok<TOk> | ResError<TError>;
