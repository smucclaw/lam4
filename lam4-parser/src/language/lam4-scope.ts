import type {LangiumCoreServices} from "langium";
import { DefaultScopeProvider, EMPTY_SCOPE, ReferenceInfo, Scope } from "langium";
import { SigDecl, isBinExpr } from "./generated/ast.js";
import { getSigAncestors, inferType, TypeEnv } from "./type-system/infer.js";
import { isSigTTag } from "./type-system/type-tags.js";

export class Lam4ScopeProvider extends DefaultScopeProvider {

  constructor(services: LangiumCoreServices) {
      super(services);
  }

  override getScope(context: ReferenceInfo): Scope {
    // not sure if this is the right property
    if (isBinExpr(context.container) && context.container.op.$type === "OpJoin") {
      console.log(context.container);
      const join = context.container;
      // if (!Object.hasOwn(join.left, "left")) return super.getScope(context);

      const typeOfLeft = inferType(new TypeEnv(), join.left);
      console.log(`left: ${typeOfLeft}`);
      const returnScope = isSigTTag(typeOfLeft) ? 
                          this.scopeSigMembers(typeOfLeft.sig) : EMPTY_SCOPE
      // When the target of our member call isn't a sig
      // This means it is either a primitive type or a type resolution error
      // Simply return an empty scope
      return returnScope;
    }

    return super.getScope(context);
  }

  private scopeSigMembers(sig: SigDecl): Scope {
    const allRelations = getSigAncestors(sig).flatMap((s: SigDecl) => s.relations);
    console.log(`relations: ${allRelations}`);
    return this.createScopeForNodes(allRelations);
  } 
}

