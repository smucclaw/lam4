import { AstNode } from "langium";
import { Project } from "./generated/ast.js";

/** For when using the default reflection-using <isPred> utils is problematic (e.g. in the scoping phase) */
export function isProjectExpr(node: AstNode): node is Project {
  return node.$type === "Project"
}

