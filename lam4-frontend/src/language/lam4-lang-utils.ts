import { AstNode } from "langium";
import { Join } from "./generated/ast.js";

/** For when using the default reflection-using <isPred> utils is problematic (e.g. in the scoping phase) */
export function isJoinExpr(node: AstNode): node is Join {
  return node.$type === "Join"
}