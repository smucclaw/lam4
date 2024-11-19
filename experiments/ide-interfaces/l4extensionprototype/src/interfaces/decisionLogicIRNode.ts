import { Result, ResError, Ok } from "./utilTypes";

/**********************
  Protocol interfaces
***********************/
export interface VisualizeIRError {
  message: string;
}

/**
JSON-RPC Request for visualizing the intermediate representation.

This is what the language server would
send to the component that actually makes the visualization.
But this is not a component that the VSCode extension would know about.
*/
export interface VisualizeDecisionLogicIRRequest {
  readonly method: "visualizeDecisionLogicIR";
  readonly params: VisualizeDecisionLogicIRInfo;
  readonly result: Result<VisualizeIRError, VisualizeDecisionLogicIRResult>;

  readonly jsonrpc: "2.0";
  readonly id: number;
}

export interface VisualizeDecisionLogicIRInfo {
  readonly program: IRNode;
}

export interface VisualizeDecisionLogicIRResult {
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
  id: number;
}

/*******************************
  Decision Logic (ish) IR node
********************************/

// Naive version where we don't bother trying to support anaphora or whatever

export type IRValue = "False" | "True" | "Unknown";

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
  readonly value: IRValue;
  readonly label: string;
}
