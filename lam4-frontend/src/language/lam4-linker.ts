/* eslint-disable  @typescript-eslint/no-explicit-any */
import {
    AstNode,
    AstNodeDescription,
    type AstNodeDescriptionProvider,
    // DefaultAstNodeDescriptionProvider,
    DefaultLinker,
    // getDocument,
    LinkingError,
    Reference,
    ReferenceInfo,

    // streamAllContents,
    type LangiumCoreServices,

} from "langium";
// import { Logger } from "tslog";
import { ANY_SCOPE } from "./lam4-scope.js";
// import { NamedElement, type Ref } from "./generated/ast.js";

// const linkerLogger = new Logger({ 
//   name: "linker",
//   prettyLogTemplate: "{{name}}  ", });


export interface UnknownWrapperRef extends AstNode {
  readonly $type: "UnknownWrapperRef";
  value: UnknownRef;
}

export interface UnknownRef extends Reference {
  readonly $type: "UnknownRef";
}


export class Lam4Linker extends DefaultLinker {
  protected nodeDescriptionProvider: AstNodeDescriptionProvider;
  constructor(services: LangiumCoreServices) {
    super(services);
    this.nodeDescriptionProvider = services.workspace.AstNodeDescriptionProvider;
    
  }

  override getCandidate(refInfo: ReferenceInfo): AstNodeDescription | LinkingError {
    const scope = this.scopeProvider.getScope(refInfo);
    if ((scope as ANY_SCOPE).scopeType === 'ANY_SCOPE') {
      // linkerLogger.debug(`ANY_SCOPE ${refInfo.reference.$refText}`);

      const refText = refInfo.reference.$refText;
      const refNode = refInfo.reference.$refNode;
      const container = refInfo.container;

      // TODO: Can't really adjust the return ref with just this
      const bareRef: UnknownRef = {
        $type: "UnknownRef",
        ref: undefined,
        $refNode: refNode,
        $refText: refText,
      }
      const wrappedRef: UnknownWrapperRef = {
        "$type": "UnknownWrapperRef",
        "value": bareRef,
        "$container": container as any, // TODO -- improve the `any`
        $containerProperty: refInfo.property
      }; 
      // linkerLogger.debug(`wrappedRef ${wrappedRef.$type} ${wrappedRef.$containerProperty}`);
      // linkerLogger.debug(`refText: ${refText}`);
      
      return this.nodeDescriptionProvider.createDescription(wrappedRef, "UnknownWrapperRef");
    }

    const description = scope.getElement(refInfo.reference.$refText);
    return description ?? this.createLinkingError(refInfo);   
  }

}
