import type { 
    // ValidationAcceptor, 
    ValidationChecks } from 'langium';
import type { Lam4AstType } from './generated/ast.js';
// import {ToplevelElement, isNamedElement, isToplevelElement} from './generated/ast.js';
import type { Lam4Services } from './lam4-module.js';

/**
 * Register custom validation checks.
 */
export function registerValidationChecks(services: Lam4Services) {
    const registry = services.validation.ValidationRegistry;
    const validator = services.validation.Lam4Validator;

    const checks: ValidationChecks<Lam4AstType> = {
        // ToplevelElement: validator.checkPredFunSigNamesUnique,
    };
    
    registry.register(checks, validator);
}


export class Lam4Validator {
    // checkPredFunSigNamesUnique(toplevelElement: ToplevelElement, accept: ValidationAcceptor): void {
    //     if (!isToplevelElement(toplevelElement)) throw new Error('Retrieved a non-top-level in validation');
        
    //     const normalize = (name: string) => name.toLowerCase();
    //     const seen = new Set<string>();

    //     if (isNamedElement(toplevelElement)) {
    //         if (seen.has(normalize(toplevelElement.name))) {
    //             accept('error',  `${toplevelElement.$type} has non-unique name '${toplevelElement.name}'.`,  {node: toplevelElement, property: 'name'});
    //         }
    //         seen.add(normalize(isNamedElement.name))
    //     }
    // }
}
