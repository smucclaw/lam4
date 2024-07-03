import type {AstNode} from "langium";
import { BinExpr} from "./generated/ast.js";

/** Checks whether the node is a Join __without__ using reflection */
export const isJoinExpr = (node?: AstNode) => node && node.$type === "BinExpr" && (node as BinExpr).op.$type === "OpJoin"; 
