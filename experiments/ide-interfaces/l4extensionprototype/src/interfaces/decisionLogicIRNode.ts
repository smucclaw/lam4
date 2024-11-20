import { JsonRPCRequest, JsonRPCMessage, Result } from "./utilTypes";

/**********************
  Protocol interfaces
***********************/

/* 
My focus was on the non-Json-RPC-specific / 'business-logic-specific' parts,
so there might be subtle JsonRPC things that I didn't get right.
But I'm hoping that won't be an issue / that this is still clear enough,
since we'll presumably be using libraries to handle that sort of plumbing.
*/

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
  /** The `Result` type might need to be tweaked to conform with JsonRPC spec --- the idea here is just that we want something like a Either type. */
  readonly result: Result<VisualizeIRError, VisualizeDecisionLogicIRResult>;
}

export interface VisualizeDecisionLogicIRInfo {
  readonly program: IRNode;
}

export interface VisualizeDecisionLogicIRResult extends JsonRPCMessage {
  readonly html: string;
}

/**********************
         IR 
************************

-----------------
  Design intent
-----------------

I framed the visualizer inputs in terms of PL concepts, 
as opposed to 'lower-level' graph visualization concepts (e.g. overlaying and connecting nodes),
because my understanding was that Matthew Waddington wanted 
the produced components to be things that other legal DSLs can also benefit from.
In particular, he seemed to want something like 
a 'L4-lite' intermediate representation whose semantics was mostly confined to propositional logic,
and that other legal DSLs can either compile to (so as, e.g., to use tools whose inputs
are in terms of this intermediate representation)
or extend with their own features.

Furthermore, if the visualizer inputs were framed in terms of, e.g., 'lower-level' graph visualization concepts,
that wouldn't be very different from visualization tools that already exist for visualizing graphs.

The other design goal was to try to come up with something that's simple while still being relatively extensible.
If there are things that do not seem easily extensible, please do not hesitate to point those out.

---------------------------------------
  Relationship to previous prototypes
---------------------------------------
In coming up with the following, I 
* looked through all the visualization-related components we've previously done (mathlangvis, Jules' ladder diagram, Meng's layman, vue pure pdpa) 
and did a quick personal assessment of their software interfaces, design intents, 
and practical pros and cons when it came to re-using them for this usecase.
 (In particular, I've run and played with most of them.)
* also looked at Maryam's https://github.com/Meowyam/lspegs

If it seems like there are aspects of extant work that have not been considered in the following,
it's likely because they are either not needed for the v1 mvp or looked like they'd have other practical disadvantages.
*/

/*****************
  Core IR node
*****************/

export interface IRNode {
  /** Discriminating property for IRNodes */
  readonly $type: string;
  /** (Stable) ID for this IRNode */
  readonly id: IRId;
  readonly annotation: IRNodeAnnotation;
}

/** Stable IDs useful for things like bidirectional synchronization down the road */
interface IRId {
  readonly id: number;
}

/** A separate record type for annotations makes it easy to add more annotation types in the future */
export interface IRNodeAnnotation {
  /** The label is what gets displayed in or around the box. */
  readonly label?: string;
}

/**
I can't think of a scenario where we'd plausibly want atomic propositions in something like a ladder diagram to not have a label;
and conversely it is easy to think of scenarios where one forgets to add the label for atomic propositions.
*/
export interface AtomicPropositionAnnotation extends IRNodeAnnotation {
  readonly label: string;
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
}

export interface Not extends IRNode {
  readonly $type: "Not";
  readonly value: IRExpr;
}

export interface AtomicProposition extends IRNode {
  readonly $type: "AtomicProposition";
  readonly value: "False" | "True" | "Unknown";
  readonly annotation: AtomicPropositionAnnotation;
}
