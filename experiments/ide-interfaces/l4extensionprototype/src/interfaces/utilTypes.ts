/*********************************
     Util types and Zod schemas
**********************************/

// From https://github.com/gvergnaud/ts-pattern/blob/b5aa458c031c08150689be8c4a2d0dc0c2b10093/tests/types-catalog/utils.ts#L78
// Doing these with Zod schemas would be too much work right now

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
