// import type { ValidationAcceptor, ValidationChecks } from 'langium';
// import type { MiniL4TypesAstType } from './generated/ast.js';
import type { MiniL4TypesServices } from './mini-l4-types-module.js';

/**
 * Register custom validation checks.
 */
export function registerValidationChecks(services: MiniL4TypesServices) {
    // TODO!
    // const registry = services.validation.ValidationRegistry;
    // const validator = services.validation.MiniL4TypesValidator;
    // const checks: ValidationChecks<MiniL4TypesAstType> = {
    //     Person: validator.checkPersonStartsWithCapital
    // };
    // registry.register(checks, validator);
}

/**
 * Implementation of custom validations.
 */
export class MiniL4TypesValidator {

    // checkPersonStartsWithCapital(person: Person, accept: ValidationAcceptor): void {
    //     if (person.name) {
    //         const firstChar = person.name.substring(0, 1);
    //         if (firstChar.toUpperCase() !== firstChar) {
    //             accept('warning', 'Person name should start with a capital.', { node: person, property: 'name' });
    //         }
    //     }
    // }

}
