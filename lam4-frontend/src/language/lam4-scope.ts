import type {LangiumCoreServices} from "langium";
import { DefaultScopeComputation, AstNode, LangiumDocument, PrecomputedScopes, DefaultScopeProvider, EMPTY_SCOPE, ReferenceInfo, Scope } from "langium";
import { Logger } from "tslog";
import { SigDecl, Join } from "./generated/ast.js";
import {isJoinExpr} from "./lam4-lang-utils.js";
import { getSigAncestors, inferType, TypeEnv } from "./type-system/infer.js";
import { isSigTTag } from "./type-system/type-tags.js";

const scopeLogger = new Logger({ 
  name: "scoper",
  prettyLogTemplate: "{{name}}  ", });


export class Lam4ScopeComputation extends DefaultScopeComputation {
  constructor(services: LangiumCoreServices) {
    super(services);
  }

  override processNode(node: AstNode, document: LangiumDocument, scopes: PrecomputedScopes): void {
    const parent = node.$container;
    const grandparent = node.$container?.$container;

    // To make params that are embedded in a ParamTypePair available to the body of predicates,
    // we need to override the default scope computation
    // In particular, need to attach the node descriptions to the grandparent instead of the parent
    // Note: CANNOT use isParam and isParamTypePair without running into issues with the reflection
    const nodeIsParamEmbeddedInParamTypePair = parent && grandparent && node.$type === "Param" && parent.$type === "ParamTypePair"
    if (nodeIsParamEmbeddedInParamTypePair) {
      const nodeName = this.nameProvider.getName(node);
      scopeLogger.debug(`scope compn ${nodeName} ${this.nameProvider.getName(parent)}`);
      scopes.add(grandparent, this.descriptions.createDescription(node, nodeName, document));
    } else {
      return super.processNode(node, document, scopes);
    }
  }
}

export class Lam4ScopeProvider extends DefaultScopeProvider {

  constructor(services: LangiumCoreServices) {
      super(services);
  }

  /**
   * General notes re how scopes are computed in Langium:
   * - Cross-references are resolved *only after* local scopes have been computed. I.e., can't rely on being able to access cross-references in the scope computaton phase. See https://langium.org/docs/reference/document-lifecycle/
   * - "A symbol in the precomputed scopes is reachable from a specific cross-reference if it is associated with a direct or indirect container of that reference."
   * - "The default implementation of the ScopeComputation service attaches the AstNodeDescription of every symbol to its direct container. This means that the container holds information about which named nodes are nested inside of it. You can override this default behavior to change the position where a symbol is reachable, or to change the name by which it can be referenced"
   */
  override getScope(context: ReferenceInfo): Scope {
    scopeLogger.trace(`(getScope) ctx property: ${context.property}; ctx reftxt: ${context.reference.$refText}`);
    scopeLogger.trace(`           self: ${context.container.$type}; parent: ${context.container.$container?.$type}`);

    const self = context.container;
    const parent = self.$container;

    const isRightChildOfJoin = parent && isJoinExpr(parent) && self === parent.right;
    if (isRightChildOfJoin) {
      const parentJoin = parent as Join;
      // getScope will have been called on parent.left before this
      // so if the left child is a Ref that can be resolved by the default linker, the Ref's reference will have been resolved
      scopeLogger.trace(`(Scope-if) ${context.reference.$refText}`);
      scopeLogger.trace(`left sib: ${parentJoin.left.$type}`)      

      // Return the members in scope in `left` iff `left` is a sig
      // (If the target of the join / field deref is not a sig, 
      // it's a primitive type or type resolution error)
      const typeOfLeft = inferType(new TypeEnv(), parentJoin.left);
      scopeLogger.trace(`           left sib ::`, typeOfLeft.toString());
      const returnScope = isSigTTag(typeOfLeft) ? 
                          this.scopeSigMembers(typeOfLeft.getSig()) : EMPTY_SCOPE
      return returnScope;
    }

    scopeLogger.trace("Not rt child of join");
    return super.getScope(context);
  }

  private scopeSigMembers(sig: SigDecl): Scope {
    const allRelations = getSigAncestors(sig).flatMap((s: SigDecl) => s.relations);
    scopeLogger.debug(`relations: ${allRelations.map(r => r.name)}`);
    return this.createScopeForNodes(allRelations);
  } 
}

