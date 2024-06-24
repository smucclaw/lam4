import { AstNode, Reference, ReferenceInfo } from "langium";
import { BinExpr, SigDecl, isBinExpr, isSigDecl, isRelation, isFunDecl} from "../generated/ast.js";
// import {Reference} from "langium/syntax-tree";

/** Get Sig ancestors via DFS */
export function getSigAncestors(sig: SigDecl): SigDecl[] {
    const seen = new Set<SigDecl>();
    const toVisit: SigDecl[] = [sig];

    while (toVisit.length > 0) {
        let next: SigDecl = toVisit.pop();
        if (!seen.has(next)) {
            seen.add(next);
            next.parents.forEach((parent: Reference<SigDecl>) => { 
                if (parent.ref) toVisit.push(parent.ref) 
            });
        }
    }

    // Sets preserve insertion order
    return Array.from(seen);
}