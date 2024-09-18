import {DefaultJsonSerializer, type LangiumCoreServices, type AstNode, } from "langium";
import type {JsonSerializeOptions} from "langium";
import {NamedElement} from "./generated/ast.js";


export class WithNamedEltRefPathJsonSerializer extends DefaultJsonSerializer {
    constructor(services: LangiumCoreServices) {
        super(services);
    }


    override replacer(key: string, value: unknown, options: JsonSerializeOptions): unknown {
        // Only need to add nodePath to NamedElements, 
        // since refs will be only to NamedElements
        if (Object.hasOwn(value as object, "name")) {
            // Might need to add more URI handling
            (value as NamedElement & {"nodePath": string})["nodePath"] = "#" + this.astNodeLocator.getAstNodePath(value as AstNode);
        }

        return super.replacer(key, value, options);
    }
}

