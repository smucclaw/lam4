import { type Module, inject } from 'langium';
import { createDefaultModule, createDefaultSharedModule, type DefaultSharedModuleContext, type LangiumServices, type LangiumSharedServices, type PartialLangiumServices } from 'langium/lsp';
import { Lam4GeneratedModule, Lam4GeneratedSharedModule } from './generated/module.js';
import { Lam4Validator, registerValidationChecks } from './lam4-validator.js';

/**
 * Declaration of custom services - add your own service classes here.
 */
export type Lam4AddedServices = {
    validation: {
        Lam4Validator: Lam4Validator
    }
}

/**
 * Union of Langium default services and your custom services - use this as constructor parameter
 * of custom service classes.
 */
export type Lam4Services = LangiumServices & Lam4AddedServices

/**
 * Dependency injection module that overrides Langium default services and contributes the
 * declared custom services. The Langium defaults can be partially specified to override only
 * selected services, while the custom services must be fully specified.
 */
export const Lam4Module: Module<Lam4Services, PartialLangiumServices & Lam4AddedServices> = {
    validation: {
        Lam4Validator: () => new Lam4Validator()
    }
};

/**
 * Create the full set of services required by Langium.
 *
 * First inject the shared services by merging two modules:
 *  - Langium default shared services
 *  - Services generated by langium-cli
 *
 * Then inject the language-specific services by merging three modules:
 *  - Langium default language-specific services
 *  - Services generated by langium-cli
 *  - Services specified in this file
 *
 * @param context Optional module context with the LSP connection
 * @returns An object wrapping the shared services and the language-specific services
 */
export function createLam4Services(context: DefaultSharedModuleContext): {
    shared: LangiumSharedServices,
    lam4Services: Lam4Services
} {
    const shared = inject(
        createDefaultSharedModule(context),
        Lam4GeneratedSharedModule
    );
    const lam4Services = inject(
        createDefaultModule({ shared }),
        Lam4GeneratedModule,
        Lam4Module
    );
    shared.ServiceRegistry.register(lam4Services);
    registerValidationChecks(lam4Services);
    return { shared, lam4Services };
}
