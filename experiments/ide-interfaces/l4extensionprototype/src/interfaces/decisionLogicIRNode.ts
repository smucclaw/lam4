import { JsonRPCRequest, JsonRPCMessage, Result } from "./utilTypes";

/**********************
  Protocol interfaces
***********************/

/** Might need fields like `code` to respect JsonRPC spec. Please mentally fill those in if so. */
export interface VisualizeIRError {
  message: string;
}

/**
JSON-RPC Request for visualizing the intermediate representation.

This is what the language server would
send to the component that actually makes the visualization.
But this is not a component that the VSCode extension would know about.
*/
export interface VisualizeDecisionLogicIRRequest extends JsonRPCRequest {
  readonly method: "visualizeDecisionLogicIR";
  readonly params: VisualizeDecisionLogicIRInfo;
  readonly result: Result<VisualizeIRError, VisualizeDecisionLogicIRResult>;
}

export interface VisualizeDecisionLogicIRInfo {
  readonly program: IRNode;
}

export interface VisualizeDecisionLogicIRResult extends JsonRPCMessage {
  readonly html: string;
}

/*****************
  Core IR node
*****************/

export interface IRNode {
  /** Discriminating property for IRNodes */
  readonly $type: string;
  /** (Stable) ID for this IRNode */
  readonly id: IRId;
}

/** Stable IDs useful for things like bidirectional synchronization down the road */
interface IRId {
  readonly id: number;
}

/*******************************
  Decision Logic (ish) IR node
********************************/

// Naive version where we don't bother trying to support anaphora or whatever

export type IRExpr = BinExpr | Not | AtomicProposition;

type BinOp = "And" | "Or";

export interface BinExpr extends IRNode {
  readonly $type: "BinExpr";
  readonly op: BinOp;
  readonly left: IRExpr;
  readonly right: IRExpr;
  readonly label?: string;
}

export interface Not extends IRNode {
  readonly $type: "Not";
  readonly value: IRExpr;
}

export interface AtomicProposition extends IRNode {
  readonly $type: "AtomicProposition";
  readonly value: "False" | "True" | "Unknown";
  /** The label is what gets displayed in or around the box */
  readonly label: string;
}
