
import { AstNode } from "langium";
import {
    SigDecl,
    BooleanLiteral,
    StringLiteral,
    NumberLiteral,
    Param,
    Relation
} from "../generated/ast.js"; 
import { zip } from "../../utils.js"
import {NodeTypePair} from "./infer.js";

export interface TypeTag {
    readonly tag: string;
    toString(): string;
    sameTypeAs(other: TypeTag): boolean;
}

/*============= Boolean ================================ */

export class BooleanTTag implements TypeTag {
    readonly tag = "Boolean";
    readonly literal?: BooleanLiteral;
    constructor(literal?: BooleanLiteral) {
        this.literal = literal;
    }
    toString() {
        return this.tag;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isBooleanTTag(other);
    }
}

export function isBooleanTTag(tag: TypeTag) {
    return tag.tag == "Boolean";
}

/*============= String ================================ */

export class StringTTag implements TypeTag {
    readonly tag = "String";
    readonly literal?: StringLiteral;
    constructor(literal?: StringLiteral) {
        this.literal = literal;
    }
    toString() {
        return this.tag;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isStringTTag(other);
    }
}

export function isStringTTag(tag: TypeTag): tag is StringTTag {
    return tag.tag === "String";
}

/*============= Unit ================================ */

export class UnitTTag implements TypeTag {
    readonly tag = "Unit";
    constructor() {}
    toString() {
        return this.tag;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isUnitTTag(other);
    }
}

export function isUnitTTag(tag: TypeTag): tag is UnitTTag {
    return tag.tag === "Unit";
}

/*============= Integer ================================ */

export class IntegerTTag implements TypeTag {
    readonly tag = "Integer";
    readonly literal?: NumberLiteral;
    constructor(literal?: NumberLiteral) {
        this.literal = literal;
    }
    toString() {
        return this.tag;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isIntegerTTag(other);
    }
}

export function isIntegerTTag(tag: TypeTag): tag is IntegerTTag {
    return tag.tag === "Integer";
}


// FOR FUTURE WORK:
/*============= Fraction ================================ */

// export class FractionTTag implements TypeTag {
//     readonly $type = "Fraction";
//     readonly astNode: AstNode;
//     constructor(astNode: AstNode) {
//         this.astNode = astNode;
//     }
//     toString() {
//         return this.$type;
//     }
// }

// export function isFractionTTag(tag: TypeTag): tag is FractionTTag {
//     return tag.$type === "Fraction";
// }

/*============= Date ================================ */

// export class DateTTag implements TypeTag {
//     readonly $type = "Date";
//     readonly astNode: AstNode;
//     constructor(astNode: AstNode) {
//         this.astNode = astNode;
//     }
//     toString() {
//         return this.$type;
//     }
// }

// export function isDateTTag(tag: TypeTag): tag is DateTTag {
//     return tag.$type === "Date";
// }

/*============= Functions and predicates ================================ */

export type FunclikeTTag = FunctionTTag | PredicateTTag;
export function isFunclikeTTag(typeTag: TypeTag): typeTag is FunclikeTTag {
    return isFunctionTTag(typeTag) || isPredicateTTag(typeTag);
}

export class FunctionTTag implements TypeTag {
    readonly tag = "Function";
    readonly returnType: TypeTag;
    private readonly parameters: FunctionParameterTypePairSequence;

    constructor(parameters: FunctionParameterTypePair[], returnType: TypeTag) {
        this.returnType = returnType;
        this.parameters = new FunctionParameterTypePairSequence(parameters);
    }
    toString() {
        return `(${this.parameters.toString()}) => ${this.returnType.toString()}`;
    }

    getParameterTypePairs(): FunctionParameterTypePairSequence {
        return this.parameters;
    }

    getReturnType() {
        return this.returnType;
    }

    sameSignatureAs(other: FunctionTTag) {
        const myParameterPairs = this.getParameterTypePairs();
        const otherParameterPairs = other.getParameterTypePairs();

        return myParameterPairs.isAlphaEquivalentTo(otherParameterPairs) && this.getReturnType().sameTypeAs(other.getReturnType());
    }

    sameTypeAs(other: TypeTag): boolean {
        return isFunctionTTag(other) && this.sameSignatureAs(other);
    }
}


export class FunctionParameterTypePairSequence {
    private readonly pairs: FunctionParameterTypePair[]
    private readonly length: number;

    constructor(pairs: FunctionParameterTypePair[]) {
        this.pairs = pairs;
        this.length = this.pairs.length;
    }

    getPairs() {
        return this.pairs;
    }

    getTypes(): TypeTag[] {
        return this.getPairs().map(p => p.getType());
    }

     /** Helper for type checking / inference: Returns the type of the param, if available
     * In a function declaration, the types are typically annotated / declared
     * before the params. That's why it's possible to encounter a param by itself
     */
    findMatchingParam(param: Param): TypeTag | null {
        const matchingPair = this.getPairs().find(
            (funparam: FunctionParameterTypePair)  => funparam.getParameter() === param);
        return matchingPair ? matchingPair.getType() : null;
    }

    isAlphaEquivalentTo(other: FunctionParameterTypePairSequence) {
        const myPairs = this.pairs;
        const otherPairs = other.getPairs();

        if (myPairs.length !== other.length) return false;
        const zipped = zip(myPairs, otherPairs);
        return zipped.every(([myParamPair, otherParamPair]) => myParamPair.sameTypeAs(otherParamPair));
    }

    // convenience method for extending TypeEnv
    asNodeTypePairs(): NodeTypePair[] {
        return this.getPairs().map(pair => ({node: pair.getParameter(), 
                                            type: pair.getType()} as NodeTypePair))
    }

    toString() {
        const params = this.getPairs().map(p => p.toString()).join(', ');
        return params;
    }
}

export class FunctionParameterTypePair {
    private readonly param: Param;
    private readonly type: TypeTag;

    constructor(param: Param, paramType: TypeTag) {
        this.param = param;
        this.type = paramType;
    }

    getParameter() {
        return this.param;
    }

    sameTypeAs(other: FunctionParameterTypePair) {
        return this.getType().sameTypeAs(other.getType())
    }

    getType() {
        return this.type;
    }

    toString() {
        return `${this.getParameter().name}: ${this.getType().toString()}`;
    }
}


export function isFunctionTTag(tag: TypeTag): tag is FunctionTTag {
    return tag.tag === "Function";
}


export class PredicateParameterTypePair extends FunctionParameterTypePair {};


// We don't need a 'PredicateContext', at least not right now, 
// because Predicates have the params embedded in ParamTypePairs

export class PredicateTTag implements TypeTag {
    readonly tag = "Predicate";
    private readonly parameters: FunctionParameterTypePairSequence;
    private readonly funTag: FunctionTTag;

    constructor(parameters: PredicateParameterTypePair[]) {
        this.funTag = new FunctionTTag(parameters, new BooleanTTag());
        this.parameters = this.funTag.getParameterTypePairs();
    }

    toString() {
        const params = this.parameters.toString();
        return `Predicate[(${params})]`;
    }

    getParameterTypePairs(): FunctionParameterTypePairSequence {
        return this.parameters;
    }

    getReturnType() { 
        return this.funTag.getReturnType();
    };

    isAlphaEquivalentTo(other: PredicateTTag) {
        const myParameterPairs = this.getParameterTypePairs();
        const otherParameterPairs = other.getParameterTypePairs();

        return myParameterPairs.isAlphaEquivalentTo(otherParameterPairs);
    }

    sameTypeAs(other: TypeTag): boolean {
        return isPredicateTTag(other) && this.isAlphaEquivalentTo(other);
    }
}


export function isPredicateTTag(tag: TypeTag): tag is PredicateTTag {
    return tag.tag === "Predicate";
}

/*============= Sig ================================ */

export class SigTTag implements TypeTag {
    readonly tag = "Sig";
    private readonly sig: SigDecl;
    constructor(sig: SigDecl) {
        this.sig = sig;
    }
    toString() {
        return `Concept ${this.sig.name}`;
    }

    getSig(): SigDecl {
        return this.sig;
    }

    sameSigAs(other: TypeTag) {
        return isSigTTag(other) && other.getSig() === this.getSig(); 
    }

    // TODO: More thought required here -- depends on desired semantics!
    sameTypeAs(other: TypeTag): boolean {
        return isSigTTag(other);
    }
    // TODO: the subtyping judgment will be interesting
}

export function isSigTTag(tag: TypeTag): tag is SigTTag {
    return tag.tag === "Sig";
}

/*============= Relation ================================== */

export class RelationTTag implements TypeTag {
    readonly tag = "Relation";
    private readonly parentSig: SigDecl;
    private readonly relationNode: Relation; 
    // A relation has type: its parent sig -> relatum (in the future, relatum_1 -> ... -> relatum_n?)
    private readonly relationType: TypeTag[];
    constructor(relationNode: Relation, parentSigType: SigTTag, relatumType: TypeTag) {
        this.relationNode = relationNode;
        this.parentSig = parentSigType.getSig();
        this.relationType = [parentSigType, relatumType];
    }
    toString() {
        return `${this.tag}: ${this.relationType}`;
    }
    getRelationNode(): Relation {
        return this.relationNode;
    }

    getRelationType(): TypeTag[] {
        return this.relationType;
    }

    // to do in the future: return relat*a*
    joinOnLeft(left: SigTTag): TypeTag | null {
        if (this.parentSig === left.getSig()) {
            return this.relationType[1];
        } else {
            return null;
        }
    }

    // TODO: More thought required here -- depends on desired semantics!
    sameTypeAs(other: TypeTag): boolean {
        // Use referential equality, since it's not possible to declare the same relation type with different sigs
        // since, on the definition above), the relation type includes as a constituent the specific Sig 
        return this === other;
    }

    // TODO: the subtyping judgment will be interesting
}

export function isRelationTTag(tag: TypeTag): tag is RelationTTag {
    return tag.tag === "Relation";
}

/*============= Error type tag ================================== */

export class ErrorTypeTag implements TypeTag {
    readonly tag = "TCError";
    readonly astNode: AstNode;
    readonly message: string;
    constructor(astNode: AstNode, message: string) {
        this.astNode = astNode;
        this.message = message;
    }
    toString() {
        return `Error: ${this.message}`;
    }

    getMessage() {
        return this.message;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isErrorTypeTag(other) && this.astNode === other.astNode && this.getMessage() === other.getMessage();
    }
}

export function isErrorTypeTag(tag: TypeTag): tag is ErrorTypeTag {
    return tag.tag === "TCError";
}