import { Reference } from "langium";
import { SigDecl } from "../generated/ast.js";
// isBinExpr, isSigDecl, isRelation, isFunDecl
// import {Reference} from "langium/syntax-tree";

/** Get Sig ancestors via DFS */
export function getSigAncestors(sig: SigDecl): SigDecl[] {
    const seen = new Set<SigDecl>();
    const toVisit: SigDecl[] = [sig];

    while (toVisit.length > 0) {
        let next: SigDecl | undefined = toVisit.pop();
        if (!next) break; // TODO: temp hack cos TS can't narrow based on length out of box

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