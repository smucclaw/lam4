import { JsonRPCRequest, JsonRPCErrorResponse, JsonRPCSuccessResponse } from "./utilTypes";

/*
---------
Questions
----------

1. Should it be automat*a* as opposed to automaton? e.g., should the VisualizeAutomatonInfo contain an array of automata as opposed to just one automaton?
*/

/**********************
  Protocol interfaces
***********************/

export interface VisualizeAutomatonRequest extends JsonRPCRequest {
  readonly method: "visualizeAutomaton";
  readonly params: VisualizeAutomatonInfo;
}

/** In the future we will want to pass in a reference
 * to something that the visualizer can use to send requests to the language server 
 * (e.g. to explain things in more detail, or to jump to the definition in the code corresponding to this state).
 * But let's keep things simple for the v1.
 */
export interface VisualizeAutomatonInfo {
  readonly automaton: Automaton;
}

export type VisualizeAutomatonResponse = VisualizeAutomatonSuccessResponse | VisualizeAutomatonErrorResponse

export interface VisualizeAutomatonResult {
  html: string;
}

export interface VisualizeAutomatonSuccessResponse extends JsonRPCSuccessResponse {
  readonly result: VisualizeAutomatonResult;
}

/** We can add more application-specific error codes etc in the future
 * (e.g. an error code for if it's somehow not a valid automaton or something)
 */
export type VisualizeAutomatonErrorResponse = JsonRPCErrorResponse;

/**********************
    Automaton IR
************************

/* If this representation seems problematic in any way,
please feel free to suggest changes! */

type StateId = number;
export interface State {
  id: StateId;
  /** Label is what potentially gets displayed to the user */
  label: string;
}

export interface Edge {
  source: StateId;
  destination: StateId;
  /** May want to make this a singular `ActionId` instead of `ActionId[]` */
  actions: ActionId[];
  // May want to add a label for the overall edge too. Not sure right now.
}

type ActionId = number;
export interface Action {
  id: number;
  /** Label is what potentially gets displayed to the user */
  label: string;
}

export interface Automaton {
  states: State[];
  transitions: Edge[];
  alphabet: Action[];
  /** Set of ids of initial states */
  initial: StateId[];
  /** Set of ids of accepting states */
  accepting: StateId[];
}