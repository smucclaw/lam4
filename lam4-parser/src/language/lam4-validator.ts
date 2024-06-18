// import type { ValidationAcceptor, ValidationChecks } from 'langium';
// import type { Lam4AstType, Model } from './generated/ast.js';
import type { Lam4Services } from './lam4-module.js';

/**
 * Register custom validation checks.
 */
export function registerValidationChecks(services: Lam4Services) {
    // const registry = services.validation.ValidationRegistry;
    // const validator = services.validation.Lam4Validator;
    // const checks: ValidationChecks<Lam4AstType> = {
    //     Person: validator.checkPersonStartsWithCapital
    // };
    // registry.register(checks, validator);
}

/**
 * Implementation of custom validations.
 */
export class Lam4Validator {

}
