import type {LangiumCoreServices} from "langium";
import { DefaultScopeComputation, AstNode, LangiumDocument, PrecomputedScopes, DefaultScopeProvider, ReferenceInfo, Reference, Scope, AstNodeDescription, EMPTY_STREAM, Stream } from "langium";
import { Logger } from "tslog";
import { RecordDecl, SigDecl, Project } from "./generated/ast.js";
import {isProjectExpr} from "./lam4-lang-utils.js";
// import { getRecordAncestors, inferType, TypeEnv } from "./type-system/infer.js";
// import { isRecordTTag } from "./type-system/type-tags.js";

const scopeLogger = new Logger({ 
  name: "scoper",
  minLevel: 3,
  prettyLogTemplate: "{{name}}  ", });

/** https://github.com/eclipse-langium/langium/discussions/856
 * > you can return some special scope object for those inferred elements, something like ANY_SCOPE, which gets special handling in the Linker implementation. 
 * > Instead of creating a referencing error, it will just create an undefined ref value. 
 * > That effectively simulates the `any` behavior in TypeScript.
 */
export class ANY_SCOPE implements Scope {
  scopeType = "ANY_SCOPE";

  constructor() {
  }

  getElement(): AstNodeDescription | undefined {
      return undefined;
  }

  getAllElements(): Stream<AstNodeDescription> {
    return EMPTY_STREAM;
  }
}

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

    const isRightChildOfProject = parent && isProjectExpr(parent) && self === parent.right;
    if (isRightChildOfProject) {
      const parentProject = parent as Project;
      
      // getScope will have been called on parent.left before this
      // so if the left child is a Ref that can be resolved by the default linker, the Ref's reference will have been resolved
      scopeLogger.trace(`(Scope-if) ${context.reference.$refText}`);
      scopeLogger.trace(`left sib: ${parentProject.left.$type}`)      

      // Sep 23 2024: Enable ANY_SCOPE for demo; disable 'type-safe record access'
      const returnScope = new ANY_SCOPE();
      return returnScope;
      
      /* Sep 23 2024: Disabling for demo 
      // Return the members in scope in `left` iff `left` is a record
      // (If the target of the project is not a record, 
      // it's a primitive type or type resolution error)
      const typeOfLeft = inferType(new TypeEnv(), parentProject.left);
      scopeLogger.trace(`           left sib ::`, typeOfLeft.toString());
      const returnScope = isRecordTTag(typeOfLeft) ? 
                          this.scopeRecordMembers(typeOfLeft.getRecord()) : EMPTY_SCOPE
      return returnScope;
      */
    }

    // scopeLogger.trace("Not rt child of Project");
    return super.getScope(context);
  }

  // Sep 2024: Deprecating this for now, till I get clearer on what should be done with Sigs
  // private scopeSigMembers(sig: SigDecl): Scope {
  //   const allRelations = getSigAncestors(sig).flatMap((s: SigDecl) => s.relations);
  //   scopeLogger.debug(`relations: ${allRelations.map(r => r.name)}`);
  //   return this.createScopeForNodes(allRelations);
  // } 

  private scopeRecordMembers(record: RecordDecl): Scope {

    const allRowTypes = getRecordAncestors(record).flatMap((record: RecordDecl) => record.rowTypes);
    scopeLogger.debug(`rowtypes: ${allRowTypes.map(r => r.name)}`);
    return this.createScopeForNodes(allRowTypes);
  }
}


// Utils copied from old infer.ts

type SigOrRecordDecl = SigDecl | RecordDecl;

function getAncestors(sigOrRecord: SigOrRecordDecl): SigOrRecordDecl[] {
  const seen = new Set<SigOrRecordDecl>();
  const toVisit: SigOrRecordDecl[] = [sigOrRecord];

  while (toVisit.length > 0) {
      const next: SigOrRecordDecl | undefined = toVisit.pop();
      if (!next) break; // TODO: temp hack cos TS can't narrow based on length out of box

      if (!seen.has(next)) {
          seen.add(next);
          next.parents.forEach((parent: Reference<SigOrRecordDecl>) => { 
              if (parent.ref) toVisit.push(parent.ref) 
          });
      }
  }

  // Sets preserve insertion order
  const seenArr = Array.from(seen);
  return seenArr;

}

export const getSigAncestors = (sig: SigDecl) => getAncestors(sig) as SigDecl[];
export const getRecordAncestors = (record: RecordDecl) => getAncestors(record) as RecordDecl[];
