/* eslint-disable */
/*
This is a simple bidirectional type checker modelled along the lines of 
that in David Christansen's tutorial.
See 
    https://davidchristiansen.dk/tutorials/bidirectional.pdf 
    https://www.youtube.com/watch?v=utyBNDj7s2w

Status, as of Jul 8 2024: 
    This __hasn't__ been thoroughly tested.
    The goal was just to get something simple 
    to support a more type-safe scoping mechanism for record field access / joins.
    
Biggest TODOs:
    - Augment Lam4 parser and type checker with support for type annotations with more structure, e.g. "(A => B) => C" 
    - Think about rewriting / desguaring FunDecls to a more convenient representation (esp. re param types)
    - Support LET
    - Support anon func
    - The type inference/checking is currently only triggered from the scoper when certain constructs are present --- need to wire it up more systematically.
    - Think about treating records as tuples or arrays / desugaring them into that
        - Keep a bidir mapping between anme of field and pos in tuple

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
                    If `f` inferred to be non-function, throw 'expected function error'
                    If error, pass error along

                    Comment: how `f` is used is not always enough to allow you to infer the type of `f`, on this algo
        * If we are able to infer the type of (f 1), call this inferred_type, 
            check whether inferred_type equal to integer.  

    Other resources:
    * https://www.reddit.com/r/ProgrammingLanguages/comments/v3z7r8/comment/ib32xpr/ points out that this does save you from having to 
    annotate function args when using higher order functions
*/
/* eslint-enable */

import { AstNode, Reference, isReference } from "langium";

import { 
    isExpr,
    Expr,
    Param,
    SigDecl, isSigDecl, StringLiteral, BooleanLiteral, IntegerLiteral,TypeAnnot, isRelation, BuiltinType, CustomTypeDef, isParamTypePair, isIfThenElseExpr, isComparisonOp, isBinExpr, BinExpr, FunDecl, isFunDecl, 
    isFunctionApplication,
    PredicateDecl,
    isPredicateDecl,
    Ref,
    Project,
    FunctionApplication,
    PostfixPredicateApplication,
    InfixPredicateApplication,
    isInfixPredicateApplication,
    UnaryExpr,
    PrimitiveTypeAnnot,
    VarBinding,
    VarDeclStmt,
    ParamTypePair,
    RecordDecl,
    isRecordDecl,
    IDOrBackTickedID,
    isRowType} from "../generated/ast.js";
import { TypeTag, ErrorTypeTag, StringTTag, IntegerTTag, isBooleanTTag, FunctionDeclTTag, isFunctionDeclTTag, PredicateTTag, isPredicateTTag, SigTTag, BooleanTTag, FunctionParameterTypePair, PredicateParameterTypePair, isErrorTypeTag, RelationTTag, UnitTTag, isUnitTTag, FunOrPredDeclTTag, ParamlessFunctionTTag, isParamlessFunctionTTag, isRecordTTag, RecordTTag, RowTypeTTag} from "./type-tags.js";
import type {FunctionParameterTypePairSequence} from "./type-tags.js";
import { isProjectExpr } from "../lam4-lang-utils.js";

import { zip } from "../../utils.js"
import { match, P } from 'ts-pattern';
import { Logger } from "tslog";


/* =====================================
 *      Logger, constants, TypeEnv 
 * ===================================== */

const typecheckLogger = new Logger({ 
    name: "tinfer",
    minLevel: 1,
    prettyLogTemplate: "{{name}}  {{logLevelName}}  "});


const ARITH_OPERATORS = ['OpPlus', 'OpMinus', 'OpMult', 'OpDiv'];
const BOOL_OPERATORS = ['OpAnd', 'OpOr', 'OpNot'];

export type NodeTypePair = {node: AstNode, type: TypeTag};

export class TypeEnv {
    private readonly envMap: Map<AstNode, TypeTag>;

    constructor(initialEnvMap = new Map()) {
        this.envMap = initialEnvMap;
    }

    lookup(node: AstNode) {
        return this.envMap.get(node) ?? null;
    }

    set(node: AstNode, type: TypeTag) {
        this.envMap.set(node, type);
    }

    /* Returns a *new* env that's extended with the input node-type pairs */
    extendWith(nodeTypePairs: NodeTypePair[]) {
        const newPairMap = new Map(nodeTypePairs.map(pair => [pair.node, pair.type]));
        const newEnvMap = new Map([...this.envMap, ...newPairMap]);
        return new TypeEnv(newEnvMap);
    }
}


/* =====================================
 *            Synth 
 * ===================================== */

export function inferType(env: TypeEnv, term: AstNode): TypeTag {

    if (!term) return new ErrorTypeTag(term, "Cannot infer type for a node that's falsy");

    const existing = env.lookup(term);
    if (existing) {
        return existing;
    } else {
        return synthNewNode(env, term);
    }
}

function synthRecord(env: TypeEnv, record: RecordDecl): TypeTag {
    const rowTypeTags = record.rowTypes.map(rtNode => ({name: rtNode.name, 
        rowType: inferType(env, rtNode.value)}));
    return  new RecordTTag(record, rowTypeTags);
}

function synthTypeAnnot(env: TypeEnv, annot: TypeAnnot | PrimitiveTypeAnnot | BuiltinType): TypeTag {
    typecheckLogger.trace(`[synthTypeAnnot]`);
    function builtinTypeAnnotToTypeTag(typeAnnot: BuiltinType) {
        const annot = typeAnnot.annot;
        switch (annot) {
            case "String":
                return new StringTTag();
            case "Integer":
                return new IntegerTTag();
            case "Boolean":
                return new BooleanTTag();
            case "Unit":
                return new UnitTTag();
            default:
                return annot as never;
        }
    }

    function customTypeAnnotToTypeTag(typeAnnot: CustomTypeDef) {
        const customType = typeAnnot.annot.ref;
        if (!customType) {
            return new ErrorTypeTag(typeAnnot, `Type annotation ${typeAnnot.annot} not found because of an issue with linking`);
        }

        return customType.$type === "RecordDecl"
                ? 
                synthRecord(env, customType as RecordDecl)
                : 
                new SigTTag(customType as SigDecl);
    }

    function isBasecaseTypeAnnot(annot: TypeAnnot | PrimitiveTypeAnnot | BuiltinType): boolean {
        return annot.$type === "BuiltinType" || annot.$type === "CustomTypeDef";
    }
    
    function isBasecaseFunctionTypeAnnot(annot: TypeAnnot | PrimitiveTypeAnnot | BuiltinType): boolean {
        return annot.$type === "TypeAnnot" && annot.right && isBasecaseTypeAnnot(annot.right);
    }    

    // Base cases
    if (annot.$type === "BuiltinType") {
        return builtinTypeAnnotToTypeTag(annot as BuiltinType);
    } else if (annot.$type === "CustomTypeDef") {
        return customTypeAnnotToTypeTag(annot as CustomTypeDef);

    // Inductive case
    } else if (annot.$type === "TypeAnnot") {
        typecheckLogger.trace(`[synthTypeAnnot -- inductive]`);
        const argTypes = [inferType(env, annot.left)];
        let rightmost = annot.right;

        while (!isBasecaseTypeAnnot(rightmost) && !isBasecaseFunctionTypeAnnot(rightmost)) {
            rightmost = (rightmost as TypeAnnot).right;

            const newArgType = inferType(env, (rightmost as TypeAnnot).left);
            argTypes.push(newArgType);
        }

        return new ParamlessFunctionTTag(argTypes, inferType(env, rightmost));

    } else {
        return new ErrorTypeTag(annot, `Type annotation ${annot} not recognized`);
    }
}

function synthBinExpr(env: TypeEnv, expr: BinExpr): TypeTag {
    typecheckLogger.trace(`[synthBinExpr]`);

    let binType: TypeTag;
    if (isBoolExpr(expr.op)) {
        binType = new BooleanTTag();
    } else if (ARITH_OPERATORS.includes(expr.op.$type)) {
        binType = new IntegerTTag();
    } else {
        binType = new ErrorTypeTag(expr, "Type of binary expression cannot be inferred");
    }
    return binType;
}


function predicateTTagFromPredDecl(env: TypeEnv, predDecl: PredicateDecl): TypeTag {
    const paramTypePairs = predDecl.params.map(pair => new PredicateParameterTypePair(pair.param, inferType(env, pair.type)));
    const predicateType = new PredicateTTag(paramTypePairs);
    return predicateType;
}


// TODO: Think about rewriting / desguaring FunDecls to a more convenient representation (esp. re param types)
function functionTTagFromFuncDecl(env: TypeEnv, fundecl: FunDecl): TypeTag {
    const paramlessFuncType = inferType(env, fundecl.funType);
    if (!isParamlessFunctionTTag(paramlessFuncType)) {
        return new ErrorTypeTag(fundecl, "A function declaration must have a function type!");
    }

    const argTypes = paramlessFuncType.getParameterTypes();
    const firstArgType = argTypes.getTypeAt(0);
    if (isUnitTTag(firstArgType)) {
        const unitParam: Param = {$container: fundecl, $type: 'Param', name: 'Unit'};
        fundecl.params.splice(0, 0, unitParam);
    }
    
    const argTypeTags: FunctionParameterTypePair[] = 
          zip(fundecl.params, argTypes.getTypes())
          .map(paramTypeTuple => new FunctionParameterTypePair(paramTypeTuple[0] as Param, paramTypeTuple[1] as TypeTag));

    const funcDeclType = new FunctionDeclTTag(argTypeTags, paramlessFuncType.getReturnType());
    typecheckLogger.trace(`[functionTTagFromFuncDecl] synth'd ${funcDeclType.toString()}`);
    return funcDeclType;
}

function synthRef(env: TypeEnv, ref: Ref): TypeTag {
    function getRefInfoForUser(ref: Ref): string {
        return `reftxt: ${ref.value.$refText}; linking error?: ${ref.value.error?.message}`
    }
    
    typecheckLogger.trace(`[synthRef] reftxt: ${ref.value.$refText}`);

    if (ref.value.ref) return inferType(env, ref.value.ref);

    typecheckLogger.debug(`Ref ${ref.value.$refText} has not been linked!`)
    return new ErrorTypeTag(ref, `Can't typecheck ref because reference can't be resolved. ${getRefInfoForUser(ref)}`)
}

/**
 * Key Precondition: If the Project is well-typed, then the reference of the leftmost child of the Project is already resolved. 
 * This is guaranteed by the structure of the Project construct in the grammar: 
 * because the depth of the leftmost child will not exceed that of the right, the scoper/linker will visit the leftmost child first
 * (and so, if the leftmost child is a Ref that can be resolved by the default linker, the Ref's reference will have been resolved) 
 */
function synthProject(env: TypeEnv, term: Project): TypeTag {
    typecheckLogger.debug(`Trying to synth left of Project: ${term.left.$type}`);
    const typeOfLeft = inferType(env, term.left);
    typecheckLogger.debug(`left of Project :: ${typeOfLeft.toString()}`);

    const rightRefName = term.right.value.$refText;
    
    // With the type of the left sibling, we can infer what the type of `left `s` right` is
    if (isRecordTTag(typeOfLeft)) {
        const recordTag = typeOfLeft;

        const memberType = recordTag.contains(rightRefName as IDOrBackTickedID);
        if (memberType) {
            typecheckLogger.debug(`(matchingMember) ${rightRefName} :: ${memberType.toString()}`);

            return memberType ?? new ErrorTypeTag(term, "a project should have worked");
            // TODO: Add checking of the synth'd type if necessary
        } else {
            return new ErrorTypeTag(term, `${rightRefName} not found in ${recordTag.getRecordName()}`)
        }
    } else {
        return new ErrorTypeTag(term.left, `The type of something to the left of a a project / field access operator should be a Record, and not a ${typeOfLeft.toString()}`)
    }
}

/*
Sep 3: The old Sig/Relations version (now currently just using Project without Join)
--------------------------------------------------------------------------------------
    // With the type of the left sibling, we can infer what the type of `left `s` right` is
    if (isSigTTag(typeOfLeft)) {
        const sig = typeOfLeft.getSig();
        const matchingRelation: Relation | undefined = sig.relations.find(relNode => relNode.name === rightRefName);
        if (matchingRelation) {
            typecheckLogger.debug("matchingRelation:", matchingRelation.name);
            const relationType = inferType(env, matchingRelation);
            typecheckLogger.debug(`(matchingRelation) ${matchingRelation.name} :: ${relationType.toString()}`);
            if (!isRelationTTag(relationType)) return new ErrorTypeTag(matchingRelation, `Relation should have a RelationTTag, but it instead has ${relationType.toString()}`);

            const joinedType = relationType.joinOnLeft(typeOfLeft);
            typecheckLogger.debug(`joinedType: ${joinedType}`);
            return joinedType ?? new ErrorTypeTag(term, "a project should have worked");
            // TODO: Add checking of the synth'd type if necessary
        } else {
            return new ErrorTypeTag(term, `Relatum ${rightRefName} not found in ${sig.name}`)
        }
    } else {
        return new ErrorTypeTag(term.left, `The type of something to the left of a a project / field access operator should be a Record, and not a ${typeOfLeft.toString()}`)
    }
*/

export function synthNewNode(env: TypeEnv, term: AstNode): TypeTag {
    typecheckLogger.trace(`--- [synthNewNode] -- term: ${term.$type}`);

    let typeTag: TypeTag = new ErrorTypeTag(term, 'Could not infer type');

    // Ref has to come first, on pain of "RangeError: Maximum call stack size exceeded"
    // also, can't use the reflection-using `isRef` without getting that error with other things that can contain Refs as children 
    // (probably has to do with how the ref resolution hasn't fully completed at this stage)
    if (term.$type === "Ref") {
        typeTag = synthRef(env, term as Ref);
    } else if (isProjectExpr(term)) {
        typeTag = synthProject(env, term as Project);

    // Constructs whose types can be easily read off the term
    } else if (term.$type == "StringLiteral") {
        typeTag = new StringTTag(term as StringLiteral);
    } else if (term.$type == "BooleanLiteral") {
        typeTag = new BooleanTTag(term as BooleanLiteral);
    } else if (term.$type == "IntegerLiteral") {
        typeTag = new IntegerTTag(term as IntegerLiteral);

    } else if (term.$type == "VarDeclStmt") {
        const varTerm = term as VarDeclStmt;
        typeTag = varTerm.varType ? inferType(env, varTerm.varType) : inferType(env, varTerm.value);
    } else if (term.$type == "VarBinding") {
        const varTerm = term as VarBinding;
        typeTag = varTerm.varType ? inferType(env, varTerm.varType) : inferType(env, varTerm.value);

    } else if (term.$type === "Param" && term.$container) {
        const parent = term.$container;
        typecheckLogger.trace(`\t\t(isParam) trying to synth param's parent, ${parent.$type}`);
        const typeOfParent = inferType(env, parent);
        typecheckLogger.trace(`\t\t(isParam) typeOfParent: ${typeOfParent.toString()}`);

        if (isParamTypePair(parent)) {
            typeTag = typeOfParent;
        } else if (isFunctionDeclTTag(typeOfParent)) {
            const paramType = typeOfParent.getParameterTypePairs().findMatchingParam(term as Param);
            typecheckLogger.trace(`paramType: ${paramType?.toString()}`);
            typeTag = paramType ?? new ErrorTypeTag(term, "param either not a param of function or lacks a type annotation");
        } else {
            typeTag = new ErrorTypeTag(term, "param either not a param of function or lacks a type annotation");
        }
    } else if (term.$type === "ParamTypePair") {
        typeTag = inferType(env, (term as ParamTypePair).type);
    } else if (term.$type === "TypeAnnot") {
        typeTag = synthTypeAnnot(env, term as TypeAnnot);
    } else if (term.$type === "CustomTypeDef") {
        typeTag = synthTypeAnnot(env, term as CustomTypeDef);
    } else if (term.$type === "BuiltinType") {
        typeTag = synthTypeAnnot(env, term as BuiltinType);
    } else if (isRecordDecl(term)) {
        typeTag = synthRecord(env, term as RecordDecl);
    } else if (isRowType(term)) {
        const fieldType = inferType(env, term.value);
        typeTag = new RowTypeTTag(term, fieldType);
    } else if (isSigDecl(term)) {
        typeTag = new SigTTag(term);
    } else if (isRelation(term)) {
        typeTag = isSigDecl(term.$container) ?
            new RelationTTag(term, (inferType(env, term.$container) as SigTTag), inferType(env, term.relatum)) :
            new ErrorTypeTag(term, "Relation should have a Concept as its parent");
    } else if (term.$type === "UnaryExpr") {
        if (isBoolExpr((term as UnaryExpr).op)) {
            typeTag = new BooleanTTag();
        } else if (ARITH_OPERATORS.includes((term as UnaryExpr).op.$type)) {
            typeTag = new IntegerTTag();
        }
    } else if (isBinExpr(term)) {
        const putativeBinType = synthBinExpr(env, term);
        typeTag = check(env, term, putativeBinType);
    } else if (isFunDecl(term)) {
        typeTag = functionTTagFromFuncDecl(env, term);
    } else if (isFunctionApplication(term)) {
        const inferredFuncType = inferType(env, term.func);
        if (isFunctionDeclTTag(inferredFuncType)) {
            typeTag = checkFunclikeApplication(env, term, inferredFuncType);
        } else {
            typeTag = new ErrorTypeTag(term.func, `We expected a function type because of the function application, but this is not a function type. (The inferred type was ${inferredFuncType.tag}`);
        }

    } else if (isPredicateDecl(term)) {
        const predType = predicateTTagFromPredDecl(env, term);
        typeTag = check(env, term, predType);
    } else if (isInfixPredicateApplication(term)) {
        if (!term.predicate.ref) {
            typeTag = new ErrorTypeTag(term, `Could not establish reference to predicate`);
        }
        
        const predType = inferType(env, term.predicate.ref as PredicateDecl);
        if (isPredicateTTag(predType)) {
            typeTag = checkFunclikeApplication(env, term, predType);   
        } else {
            typeTag = new ErrorTypeTag(term.predicate.ref as PredicateDecl, `We expected a predicate type because of the predicate application, but this is not a predicate type.`);
        }

    } else {
        typeTag = new ErrorTypeTag(term, `Type of term cannot be inferred. Note that Let and Anon Func haven't been implemented`);
    }

    env.set(term, typeTag);
    return typeTag;
}

/* ===========================
 *      Check 
 * =========================== */

export type FunclikeApplication = InfixPredicateApplication | PostfixPredicateApplication | FunctionApplication;

/** Check that Γ |- arg : expected type, for each of the (arg, expected type) pairs */
function checkFunclikeApplication(env: TypeEnv, term: FunclikeApplication, inferredFunOrPredDeclTTag: FunOrPredDeclTTag) {
    let typeTag: TypeTag  = new ErrorTypeTag(term, 'Could not check fun / pred application');
    const inferredArgTypes = inferredFunOrPredDeclTTag.getParameterTypePairs().getTypes();

    // Desugar the args
    let args;
    if (isInfixPredicateApplication(term)) {
        args = [term.left, term.right].filter(arg => isExpr(arg)) as Expr[];
    } else {
        args = term.args.map(arg => isReference(arg) ? arg.ref : arg).filter(arg => arg) as AstNode[];
    }

    // arg length check
    if (args.length !== inferredArgTypes.length) {
        return new ErrorTypeTag(term, "Number of args do not match the number of declared params (there may have been an unlinked reference)");
    }
    
    // Check that the synthed types agree with the expected types
    let checkPassed = true;
    for (const [arg, expectedType] of zip(args, inferredArgTypes)) {
        const checkRetType = check(env, arg as AstNode, expectedType as TypeTag);
        if (isErrorTypeTag(checkRetType)) {
            typeTag = checkRetType;
            checkPassed = false;
        }
    }
    if (checkPassed) typeTag = inferredFunOrPredDeclTTag.getReturnType();
    
    return typeTag;
}

/** Check that 
 * 
 *    Γ, x: t_1 |- b <= t_2
 * ---------------------------
 * Γ |- lam x. b <= t_1 -> t_2
 * 
 * */
function checkFunType(env: TypeEnv, parameters: FunctionParameterTypePairSequence, returnType: TypeTag, body: AstNode) {
    const newExtendedEnv = env.extendWith(parameters.asNodeTypePairs());
    return check(newExtendedEnv, body, returnType);
}

/* eslint-disable */
const check = (env: TypeEnv, term: AstNode, type: TypeTag): TypeTag =>
    match([term, type])
        .with([P._, P.when(isErrorTypeTag)],
            ([_, type]) => type)
        .with([P.when(isBinExpr), P._],
            ([binE, type]) => {

                function checkChildrenHaveSameType(env: TypeEnv, expr: BinExpr, type: TypeTag): TypeTag {
                    const leftType = check(env, expr.left, type);
                    const rightType = check(env, expr.right, type);
                    const typesCheck = [leftType, rightType].every(typeTag => typeTag.sameTypeAs(type));
                    return typesCheck ? type : new ErrorTypeTag(expr, `Type error in binary expression. Expected type to be ${type}, but got ${leftType} and ${rightType}`);
                }
                return checkChildrenHaveSameType(env, binE, type);
            })

        .with([P.when(isFunDecl), P.when(isFunctionDeclTTag)],
            ([fundecl, type]) => checkFunType(env, type.getParameterTypePairs(), type.getReturnType(), fundecl.body))
        .with([P.when(isFunDecl), P._],
            ([fundecl, _]) => new ErrorTypeTag(fundecl, "Function declaration must be annotated with a function type"))

        .with([P.when(isPredicateDecl), P.when(isPredicateTTag)],
            ([predDecl, type]) => checkFunType(env, type.getParameterTypePairs(), new BooleanTTag(), predDecl.body))
        .with([P.when(isPredicateDecl), P._],
            ([predDecl, _]) => new ErrorTypeTag(predDecl, 
                                                "Predicate decl must be annotated with a predicate type"))

        .with([P.when(isIfThenElseExpr), P._], 
            ([ite, _]) => { 
                const iteChecksPass = isBooleanTTag(inferType(env, ite.condition)) 
                                        && inferType(env, ite.then).sameTypeAs(type)
                                        && inferType(env, ite.else).sameTypeAs(type); 
                // TODO: Could do more error checking and more detailed error messages
                return iteChecksPass ? type : new ErrorTypeTag(ite, "Type error in if-then-else expression (a more helpful error msg is possible with more work)");
            })
        .otherwise(([term, type]) => {
            const termType = inferType(env, term);
            return termType.sameTypeAs(type) ? type : new ErrorTypeTag(term, "Type error");
            // TODO in the future: isSubtype(termType, type) ? type : new ErrorTypeTag(term, "Type error");
        });
/* eslint-enable */

/* ===========================
 *      Utils 
 * =========================== */

const isBoolExpr = (op: AstNode) => isComparisonOp(op) || (BOOL_OPERATORS.includes(op.$type));

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
    typecheckLogger.silly('seenArr:', seenArr.map(sigOrRecord => sigOrRecord.name));

    return seenArr;

}

export const getSigAncestors = (sig: SigDecl) => getAncestors(sig) as SigDecl[];
export const getRecordAncestors = (record: RecordDecl) => getAncestors(record) as RecordDecl[];

// /** Get Sig ancestors via DFS */
// export function getSigAncestors(sig: SigDecl): SigDecl[] {
//     const seen = new Set<SigDecl>();
//     const toVisit: SigDecl[] = [sig];

//     while (toVisit.length > 0) {
//         let next: SigDecl | undefined = toVisit.pop();
//         if (!next) break; // TODO: temp hack cos TS can't narrow based on length out of box

//         if (!seen.has(next)) {
//             seen.add(next);
//             next.parents.forEach((parent: Reference<SigDecl>) => { 
//                 if (parent.ref) toVisit.push(parent.ref) 
//             });
//         }
//     }

//     // Sets preserve insertion order
//     const seenArr = Array.from(seen);
//     typecheckLogger.silly('seenArr:', seenArr.map(sig => sig.name));

//     return seenArr;
// }


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
