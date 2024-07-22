
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

// ----- ParamlessFunctionTTag, FunctionDeclTTag, FunOrPredDeclTTag -----

export type FunOrPredDeclTTag = FunctionDeclTTag | PredicateTTag;

export abstract class FunctionTTag {
    abstract getReturnType(): TypeTag;
    abstract getParameterTypes(): FunctionParameterTypeSequence;   
    sameSignatureAs(other: FunctionTTag): boolean {
        const myParameters = this.getParameterTypes();
        const otherParameters = other.getParameterTypes();

        return myParameters.isAlphaEquivalentTo(otherParameters) && this.getReturnType().sameTypeAs(other.getReturnType());
    }
    toString() {
        return `(${this.getParameterTypes().toString()}) => ${this.getReturnType.toString()}`;
    }
}

/** A function type that doesn't record the Params -- just the argument and return types. 
 * Meant for use with type annotations */
export class ParamlessFunctionTTag extends FunctionTTag implements TypeTag {
    readonly tag = "Function";
    readonly returnType: TypeTag;
    private readonly parameters: FunctionParameterTypeSequence;

    constructor(parameters: TypeTag[], returnType: TypeTag) {
        super();
        this.returnType = returnType;
        this.parameters = new FunctionParameterTypeSequence(parameters);
    }

    getParameterTypes(): FunctionParameterTypeSequence {
        return this.parameters;
    }

    getReturnType() {
        return this.returnType;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isBaseFunctionTTag(other) && this.sameSignatureAs(other);
    }
}


export class FunctionDeclTTag extends FunctionTTag implements TypeTag {
    readonly tag = "FunctionDecl";
    private readonly baseFunTag: ParamlessFunctionTTag
    private readonly parameters: FunctionParameterTypePairSequence;

    constructor(parameters: FunctionParameterTypePair[], returnType: TypeTag) {
        super();
        this.parameters = new FunctionParameterTypePairSequence(parameters);
        this.baseFunTag = new ParamlessFunctionTTag(parameters.map(p => p.getType()), returnType);
    }

    override toString() {
        return `(${this.parameters.toString()}) => ${this.getReturnType().toString()}`;
    }

    getParameterTypes(): FunctionParameterTypeSequence {
        return this.baseFunTag.getParameterTypes()
    }

    getParameterTypePairs(): FunctionParameterTypePairSequence {
        return this.parameters;
    }

    getReturnType() {
        return this.baseFunTag.getReturnType();
    }

    sameTypeAs(other: TypeTag): boolean {
        return isFunctionDeclTTag(other) && this.sameSignatureAs(other);
    }
}

// ----- FunctionParameter related -------------

export class FunctionParameterTypeSequence {
    private readonly types: TypeTag[]
    private readonly length: number;

    constructor(types: TypeTag[]) {
        this.types = types;
        this.length = this.types.length;
    }

    getTypes() {
        return this.types;
    }

    getTypeAt(index: number) {
        return this.types[index];
    }

    isAlphaEquivalentTo(other: FunctionParameterTypeSequence) {
        const myTypes = this.types;
        const otherTypes = other.getTypes();

        if (myTypes.length !== other.length) return false;

        const zipped = zip(myTypes, otherTypes);
        return zipped.every(([myType, otherType]) => myType.sameTypeAs(otherType));
    }
}

export class FunctionParameterTypePairSequence extends FunctionParameterTypeSequence {
    private readonly pairs: FunctionParameterTypePair[]

    constructor(pairs: FunctionParameterTypePair[]) {
        const types = pairs.map(p => p.getType());
        super(types);

        this.pairs = pairs;
    }

    getPairs() {
        return this.pairs;
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

    // convenience method for extending TypeEnv
    asNodeTypePairs(): NodeTypePair[] {
        return this.getPairs().map(pair => ({node: pair.getParameter(), 
                                            type: pair.getType()} as NodeTypePair))
    }

    override toString() {
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

// ----- type guard predicates ------------------

export function isFunOrPredDeclTTag(typeTag: TypeTag): typeTag is FunOrPredDeclTTag {
    return isFunctionDeclTTag(typeTag) || isPredicateTTag(typeTag);
}

export function isParamlessFunctionTTag(typeTag: TypeTag): typeTag is ParamlessFunctionTTag {
    return typeTag.tag === "Function";
}

export function isBaseFunctionTTag(tag: TypeTag): tag is ParamlessFunctionTTag {
    return tag.tag === "Function";
}

export function isFunctionDeclTTag(tag: TypeTag): tag is FunctionDeclTTag {
    return tag.tag === "FunctionDecl";
}

// ----- Predicates ------------------

export class PredicateParameterTypePair extends FunctionParameterTypePair {};


export class PredicateTTag extends FunctionTTag implements TypeTag {
    readonly tag = "Predicate";
    private readonly parameters: FunctionParameterTypePairSequence;
    private readonly funTag: FunctionDeclTTag;

    constructor(parameters: PredicateParameterTypePair[]) {
        super();
        this.funTag = new FunctionDeclTTag(parameters, new BooleanTTag());
        this.parameters = this.funTag.getParameterTypePairs();
    }

    override toString() {
        const params = this.parameters.toString();
        return `Predicate[(${params})]`;
    }

    getParameterTypePairs(): FunctionParameterTypePairSequence {
        return this.parameters;
    }

    getParameterTypes() {
        return this.funTag.getParameterTypes();
    }

    getReturnType() { 
        return this.funTag.getReturnType();
    };

    sameTypeAs(other: TypeTag): boolean {
        return isPredicateTTag(other) && this.sameSignatureAs(other);
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