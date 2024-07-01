import { AstNode, Reference } from "langium";

import { 
    // isLiteral, Literal, 
    SigDecl, isSigDecl, isStringLiteral, StringLiteral, isBooleanLiteral, BooleanLiteral, NumberLiteral, isNumberLiteral, isVarDecl, isTypeAnnot, TypeAnnot, isRelation, isBuiltinType, BuiltinType, isCustomTypeDef, CustomTypeDef, isParamTypePair, isIfThenElseExpr, IfThenElseExpr, isComparisonOp, isBinExpr, BinExpr, FunDecl, isFunDecl, 
    isFunctionApplication,
    FunctionApplication,
    PredicateDecl,
    isPredicateDecl,
    isRef} from "../generated/ast.js";
import { TypeTag, ErrorTypeTag, StringTTag, IntegerTTag, isBooleanTTag, FunctionTTag, isFunctionTTag, PredicateTTag, isPredicateTTag, SigTTag, BooleanTTag, FunctionParameter, isErrorTypeTag, isSigTTag} from "./type-tags.js";
import { 
    // Either, Failure, Success, 
    zip } from "../../utils.js"
import { match, P } from 'ts-pattern';

/*
TODOs: 
* Handle predicates etc -- pred app the main thing left
* Add more typechecking for the Join / record access later
* isSubtypeOf
*/

const ARITH_OPERATORS = ['OpPlus', 'OpMinus', 'OpMult', 'OpDiv'];

type NodeTypePair = {node: AstNode, type: TypeTag};
// Branded<{node: AstNode, type: TypeTag}, "NodeTypePair">;

export class TypeEnv {
    private readonly envMap: Map<AstNode, TypeTag>;

    constructor(initialEnvMap = new Map()) {
        this.envMap = initialEnvMap;
    }

    lookup(node: AstNode) {
        return this.envMap.get(node) ?? null;
    }

    add(node: AstNode, type: TypeTag) {
        this.envMap.set(node, type);
    }

    /* Returns a *new* env that's extended with the input node-type pairs */
    extendWith(nodeTypePairs: NodeTypePair[]) {
        const newPairMap = new Map(nodeTypePairs.map(pair => [pair.node, pair.type]));
        const newEnvMap = new Map([...this.envMap, ...newPairMap]);
        return new TypeEnv(newEnvMap);
    }
}


export function inferType(env: TypeEnv, term: AstNode): TypeTag {

    if (!term) return new ErrorTypeTag(term, "Cannot infer type for a node that's falsy");

    const existing = env.lookup(term);
    if (existing) {
        return existing;
    } else {
        return inferTypeOfNewNode(env, term);
    }
}

// TODO placeholder
// const isSubtype = (type1: TypeTag, type2: TypeTag): boolean => {

// }; 

const check = (env: TypeEnv, term: AstNode, type: TypeTag): TypeTag =>
    match(term)
        // literals
        // .with(P.when(isLiteral),
        //     (lit: Literal) => inferType(env, lit).sameTypeAs(type) ? type : new ErrorTypeTag(lit, "Type mismatch"))
        
        .with(P.when(isFunDecl),
            (fundecl: FunDecl) => {
                if (!isFunctionTTag(type)) return new ErrorTypeTag(fundecl, "Function declaration must be annotated with a function type");

                const funcParamTypePairs: FunctionParameter[] = type.parameters;
                const funcRetType = type.returnType;
                const newExtendedEnv = env.extendWith(
                                funcParamTypePairs.map(pair => ({node: pair.param, 
                                                                type: pair.type} as NodeTypePair)));
                return check(newExtendedEnv,
                             fundecl.body, 
                             funcRetType);
            })
        .with(P.when(isPredicateDecl),
            (predDecl: PredicateDecl) => {
                if (!isPredicateTTag(type)) return new ErrorTypeTag(predDecl, 
                                            "Predicate decl must be annotated with a predicate type");

                const newExtendedEnv = env.extendWith(
                        type.parameters.map(pair => ({node: pair.param, 
                        type: pair.type} as NodeTypePair)));
                        
                return check(newExtendedEnv, predDecl.body, new BooleanTTag());
            })
        .with(P.when(isIfThenElseExpr), 
            (ite: IfThenElseExpr) => { 
                const iteChecksPass = isBooleanTTag(inferType(env, ite.condition)) 
                                        && inferType(env, ite.then).sameTypeAs(type)
                                        && inferType(env, ite.else).sameTypeAs(type); 
                // TODO: Could do more error checking and more detailed error messages
                return iteChecksPass ? type : new ErrorTypeTag(ite, "Type error in if-then-else expression (a more helpful error msg is possible with more work)");
            })
        
        .otherwise(term => {
            let termType = inferType(env, term);
            return termType.sameTypeAs(type) ? type : new ErrorTypeTag(term, "Type error");
            // isSubtype(termType, type) ? type : new ErrorTypeTag(term, "Type error");
        });


function inferTypeAnnot(env: TypeEnv, typeAnnot: TypeAnnot): TypeTag {
    //ts-pattern can't seem to work with the reflection?

    function builtinTypeAnnotToTypeTag(typeAnnot: BuiltinType) {
        const annot = typeAnnot.annot;
        switch (annot) {
            case "String":
                return new StringTTag();
            case "Integer":
                return new IntegerTTag();
            case "Boolean":
                return new BooleanTTag();
            default:
                return annot as never;
        }
    }

    function customTypeAnnotToTypeTag(typeAnnot: CustomTypeDef) {
        const customType = typeAnnot.annot.ref;
        if (!customType) {
            return new ErrorTypeTag(typeAnnot, `Type annotation ${typeAnnot.annot} not found because of an issue with linking`);
        }
        return new SigTTag(customType as SigDecl);
    }


    // Get the type annot
    if (isBuiltinType(typeAnnot)) {
        return builtinTypeAnnotToTypeTag(typeAnnot);
    } else if (isCustomTypeDef(typeAnnot)) {
        return customTypeAnnotToTypeTag(typeAnnot);
    } else {
        return new ErrorTypeTag(typeAnnot, `Type annotation ${typeAnnot} not recognized`);
    }
}

/*
Some intuition on bidirectional typechecking, for those unfamiliar with it:
-----------------------------------------------------------------------------
    How would
       infer ( (f 1) + 2 )
    go?

    1. Infer that (f 1) and 2 should be integers
    2. Check whether (f 1) is an integer
    3. => Try to -- infer the type of (f 1) ---.
            * (Infer fun app) 
                    Try to infer quid `f`; if it's inferred to be a function type of form (tfun a b), check that the actual argument `1` has the inferred type `a`
                    If `f` inferred to be non-function, throw 'expected function erorr'
                    If error, pass error along

                    Comment: how `f` is used is not enough to allow you to infer the type of `f`, on this algo
        * If we are able to infer the type of (f 1), call this inferred_type, 
            check whether inferred_type equal to integer.  

    Other resources:
    * https://www.reddit.com/r/ProgrammingLanguages/comments/v3z7r8/comment/ib32xpr/ points out that this saves you from having to 
    annotate function args when using higher order functions
*/

function inferBinExpr(env: TypeEnv, expr: BinExpr): TypeTag {
    function checkChildren(env: TypeEnv, expr: BinExpr, type: TypeTag): TypeTag {
        const leftType = check(env, expr.left, type);
        const rightType = check(env, expr.right, type);
        const typesCheck = [leftType, rightType].every(typeTag => typeTag.sameTypeAs(type));
        return typesCheck ? type : new ErrorTypeTag(expr, `Type error in binary expression. Expected type to be ${type}, but got ${leftType} and ${rightType}`);
    }

    const isBoolExpr = isComparisonOp(expr.op) || (['OpAnd', 'OpOr'].includes(expr.op.$type));
    let binType: TypeTag;
    if (isBoolExpr) {
        // TODO: Not sure if should check for whether the arguments are indeed boolean exprs here, or in `lam4-validator` 
        binType = new BooleanTTag();
        return checkChildren(env, expr, binType);
    } else if (ARITH_OPERATORS.includes(expr.op.$type)) {
        binType = new IntegerTTag();
        return checkChildren(env, expr, binType);
    } else if (expr.op.$type === "OpJoin") {
        // join, aka record dereference/access (for now)
        const inferredLeft = inferType(env, expr.left);
        if (isSigTTag(inferredLeft) && !Object.hasOwn(expr.left, "left")) {
            // const relation = inferredLeft.sig.relations.find(rel => rel.name === expr.right.name)
            console.log(`[Join] trying to infer type of expr.right ${expr.right}`)
            return inferType(env, expr.right);
        } else {
            return new ErrorTypeTag(expr.left, "Expected something that evaluates to a 'CONCEPT' on the left side of the `s`");
        }

    } else {
        return new ErrorTypeTag(expr, "Type of binary expression cannot be inferred");
    }
}


        
        // right now I'm treating this *just* as a record, but will try to get the relations semantics going in v2 / v3
        // .with(P.when(isJoin),
        //         join => {
        //             // TODO: Add more typechecking later
        //             const inferredLeft = inferType(env, join.left);
                    
        //         })


function predicateTTagFromPredDecl(env: TypeEnv, predDecl: PredicateDecl): TypeTag {
    const paramTypePairs = predDecl.params.map(pair => ({param: pair.param, type: inferTypeAnnot(env, pair.type)} as FunctionParameter));
    const predicateType = new PredicateTTag(paramTypePairs);
    return predicateType;
}

function functionTTagFromFuncDecl(env: TypeEnv, fundecl: FunDecl): TypeTag {
    const paramAndRetTypes = fundecl.types.map(inferType.bind(undefined, env));
    const argTypes = paramAndRetTypes.slice(0, -1);
    const retType = paramAndRetTypes[paramAndRetTypes.length - 1];

    if (argTypes.length !== fundecl.params.length) return new ErrorTypeTag(fundecl, "Number of parameters and parameter types do not match");
    
    const argTypeTags: FunctionParameter[] = 
          zip(fundecl.params, argTypes)
          .map(paramTypeTuple => ({param: paramTypeTuple[0], type: paramTypeTuple[1]} as FunctionParameter));

    const funcType = new FunctionTTag(argTypeTags, retType);
    return funcType;
}

export function inferTypeOfNewNode(env: TypeEnv, term: AstNode): TypeTag {
    // TODO: Not sure this is needed
    // Forestall recursive inference errors
    //  env.set(node, new ErrorType(node, 'Recursive inference error'));
    let typeTag: TypeTag;
    console.log(`term: ${term}`);
    typeTag = match(term)

        // The types that can be read off the exprs
        .with(P.when(isStringLiteral), 
                (node: AstNode) => new StringTTag(node as StringLiteral))
        .with(P.when(isBooleanLiteral),
                node => new BooleanTTag(node as BooleanLiteral))
        .with(P.when(isNumberLiteral),
                node => new IntegerTTag(node as NumberLiteral)
                    // only integers for first pass of type infer / check
                    // if (Number.isInteger(node.value)) {
                    //     typeTag = new IntegerTTag(node);
                    // } else {
                    //     typeTag = new FractionTTag(node);
                    // }
                )

        .with(P.when(isVarDecl),
                node => (node.varType) 
                    ? inferType(env, node.varType) 
                    : inferType(env, node.value))
        .with(P.when(isParamTypePair),
                node => inferType(env, node.type))
        .with(P.when(isTypeAnnot),
                typeAnnot => inferTypeAnnot(env, typeAnnot))    
        .with(P.when(isRef),
                ref => ref.value.ref 
                        ? inferType(env, ref.value.ref) 
                        : new ErrorTypeTag(term, `Can't typecheck ${term} because reference can't be resolved`))

        .with(P.when(isSigDecl),
                sig => new SigTTag(sig))
        .with(P.when(isRelation),
                relation => inferType(env, relation.relatum))

        .with(P.when(isBinExpr),
                binExpr => inferBinExpr(env, binExpr))

        .with(P.when(isFunDecl),
                fundecl => {
                    // Propagate fn type annotation and check
                    const funcType = functionTTagFromFuncDecl(env, fundecl);
                    return check(env, fundecl, funcType);
                })
        .with(P.when(isFunctionApplication),
                (funApp: FunctionApplication) => {
                    const inferredFuncType = inferType(env, funApp.func);
                    if (isFunctionTTag(inferredFuncType)) {
                        // Check that Γ |- arg : expected type, for each of the (arg, expected type) pairs
                        const inferredArgTypes = inferredFuncType.parameters.map(p => p.type);
                        for (const [arg, expectedType] of zip(funApp.args, inferredArgTypes)) {
                            const checkRetType = check(env, arg as AstNode, expectedType as TypeTag);
                            if (isErrorTypeTag(checkRetType)) {
                                return checkRetType;
                            }
                        }

                        return inferredFuncType.returnType;
                    } else {
                        return new ErrorTypeTag(funApp.func, "We expected a function type because of the function application, but this is not a function type");
                    }

                })
        .with(P.when(isPredicateDecl),
                predDecl => {
                    const predType = predicateTTagFromPredDecl(env, predDecl);
                    return check(env, predDecl, predType);
                })    
        .otherwise(() => new ErrorTypeTag(term, "Type of term cannot be inferred"));

    env.add(term, typeTag);
    return typeTag;
}

/** Get Sig ancestors via DFS */
export function getSigAncestors(sig: SigDecl): SigDecl[] {
    const seen = new Set<SigDecl>();
    const toVisit: SigDecl[] = [sig];

    while (toVisit.length > 0) {
        let next: SigDecl | undefined = toVisit.pop();
        if (!next) break; // TODO: temp hack cos TS can't narrow based on length out of box

        if (!seen.has(next)) {
            seen.add(next);
            next.parents.forEach((parent: Reference<SigDecl>) => { 
                if (parent.ref) toVisit.push(parent.ref) 
            });
        }
    }

    console.log(`seen: ${seen}`)

    // Sets preserve insertion order
    return Array.from(seen);
}


/*============= Error ================================ */
// class TCError {
//     readonly tag = "TCError";
//     readonly node: AstNode;
//     readonly message: string;

//     constructor(node: AstNode, message: string) {
//         this.node = node;
//         this.message = message;
//     }

// }
// type CheckResult = Either<TCError, TypeTag>;

// TODO: This is hacky -- figure out what a better way of handling type errors is, after the first draft
// function tcErrorToTypeTag(tcError: TCError): TypeTag {
//     return new ErrorTypeTag(tcError.astNode, tcError.message);
// }

// const unwrapResult = (either: Either<TCError, TypeTag>): TypeTag =>
//      match(either)
//         .with({tag: "failure"}, (either) => tcErrorToTypeTag(either.value))
//         .with({tag: "success"}, (either) => either.value)
//         .exhaustive();
