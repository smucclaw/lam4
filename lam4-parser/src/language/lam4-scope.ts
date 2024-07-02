import type {LangiumCoreServices} from "langium";
import { DefaultScopeProvider, EMPTY_SCOPE, ReferenceInfo, Scope } from "langium";
import { BinExpr, SigDecl, isBinExpr } from "./generated/ast.js";
import { getSigAncestors, inferType, TypeEnv } from "./type-system/infer.js";
import { isSigTTag } from "./type-system/type-tags.js";

export class Lam4ScopeProvider extends DefaultScopeProvider {

  constructor(services: LangiumCoreServices) {
      super(services);
  }

  override getScope(context: ReferenceInfo): Scope {
    console.log(`getScope: ctx property: ${context.property}\n container with type ${context.container.$type}; its parent container has type ${context.container.$container?.$type}`);

    /*
    x `s`: getScope: ctx property: value
                        container with type Ref; its parent container has type BinExpr
    x `s` a: getScope: ctx property: value
             container with type Ref; its parent container has type Ref
    */            

    // ?? This breaks even something like
    // Yo => Integer
    // f(lifeAssured) = lifeAssured 
    // BUT WHY WOULD IT BREAK THAT?

    // TODO: not sure if this is the right property; went for what's most analogous
    // Suppose we're trying to figure out the scope for `right` of a join
    // context.container.$type === "BinExpr" && ((context.container) as BinExpr).op.$type === "OpJoin"
    if (context.container.$type === "Ref" && context.container.$container?.$type === "BinExpr") {
      console.log("in if");
      const self = context.container;
      const join = context.container.$container as BinExpr;

      // leftmost
      if (join.left === self && !Object.hasOwn(self, "left")) {
        console.log('!Object.hasOwn(join.left, "left")');
        return super.getScope(context);
      }

      // inductive step: This is a join where the `left` itself has another `left` sibling
      const typeOfLeft = inferType(new TypeEnv(), join.left);
      console.log(`left: ${typeOfLeft}`);
      const returnScope = isSigTTag(typeOfLeft) ? 
                          this.scopeSigMembers(typeOfLeft.sig) : EMPTY_SCOPE
      // // When the target of our member call isn't a sig
      // // This means it is either a primitive type or a type resolution error
      // // Simply return an empty scope
      return returnScope;
    }

    return super.getScope(context);
  }

  private scopeSigMembers(sig: SigDecl): Scope {
    console.log("scopeSigMembers");
    const allRelations = getSigAncestors(sig).flatMap((s: SigDecl) => s.relations);
    console.log(`relations: ${allRelations}`);
    return this.createScopeForNodes(allRelations);
  } 
}

