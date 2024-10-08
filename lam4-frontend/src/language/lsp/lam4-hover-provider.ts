import { AstNode } from "langium";
import { Hover } from "vscode-languageclient";
import { isSigDecl, 
    // isNamedElement, 
    isRecordDecl } from "../generated/ast.js";
// import { isErrorTypeTag } from "../type-system/type-tags.js";
// import { inferType, TypeEnv } from "../type-system/infer.js";
import { AstNodeHoverProvider } from "langium/lsp";

export class Lam4HoverProvider extends AstNodeHoverProvider {
  protected getAstNodeHoverContent(node: AstNode): Hover | undefined {
      if (isSigDecl(node)) {
          return {
              contents: {
                  kind: 'markdown',
                  language: 'lam4',
                  value: `Concept ${node.name} ${node.parents ? ` ${node.parents}` : ''}`
              }
          }
    } else if (isRecordDecl(node)) {
        return {
            contents: {
                kind: 'markdown',
                language: 'lam4',
                value: `Structure ${node.name} ${node.parents ? `specializing ${node.parents}` : ''}`
            }
        }
    } else {
        return {
            contents: {
                kind: 'markdown',
                language: 'lam4',
                value: `${node.$type}`          
            }
        }
    }
    // Disable type checker for demo
    // } else if (isNamedElement(node)) {
    //       const type = inferType(new TypeEnv(), node);
    //       if (isErrorTypeTag(type)) {
    //           return {
    //             contents: {
    //               kind: 'markdown',
    //               language: 'lam4',
    //               value: type.toString()}
    //           }
    //       }
    //       return {
    //           contents: {
    //               kind: 'markdown',
    //               language: 'lam4',
    //               value: `${node.name} is a ${type.toString()}`
    //           }
    //       }
    //   }

  }
}