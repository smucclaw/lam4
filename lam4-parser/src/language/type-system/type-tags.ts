import { toString } from 'langium/generate';
import { AstNode } from "langium";
import {
    SigDecl,
    BooleanLiteral,
    StringLiteral,
    NumberLiteral,
    Param,
    Relation,
    isRelation,
} from "../generated/ast.js"; 

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

/*============= Function ================================ */


export class FunctionTTag implements TypeTag {
    readonly tag = "Function";
    readonly returnType: TypeTag;
    private readonly parameters: FunctionParameter[];
    constructor(parameters: FunctionParameter[], returnType: TypeTag) {
        this.returnType = returnType;
        this.parameters = parameters;
    }
    toString() {
        const params = this.parameters.map(p => p.toString()).join(', ');
        return `(${params}) => ${this.returnType.toString()}`;
    }

    getParameters(): FunctionParameter[] {
        return this.parameters;
    }

    getReturnType() {
        return this.returnType;
    }

    // Make a ParamSeq class too? argh
    getTypeOfParam(param: Param): TypeTag | null {
        const matchingFunparam = this.parameters.find(funparam => funparam.getParam() === param);
        return matchingFunparam ? matchingFunparam.getType() : null;

    }

    private paramTagsCoincide(other: TypeTag) {
        if (!isFunctionTTag(other)) return false;

        if (this.parameters.length !== other.parameters.length) return false;  
        for (let i = 0; i < this.parameters.length; i++) {
            if (!this.parameters[i].getType().sameTypeAs(other.parameters[i].getType())) return false;
        }
        return this.getReturnType().sameTypeAs(other.getReturnType());
    }

    sameTypeAs(other: TypeTag): boolean {
        return this.paramTagsCoincide(other);
    }
}

export class PredicateTTag implements TypeTag{
    readonly tag = "Predicate";
    private readonly parameters: PredicateParameter[];
    private readonly funTag: FunctionTTag;

    constructor(parameters: PredicateParameter[]) {
        this.parameters = parameters;

        this.funTag = new FunctionTTag(parameters, new BooleanTTag());
    }

    toString() {
        const params = this.parameters.map(p => p.toString()).join(', ');
        return `Predicate[(${params})]`;
    }
    getParameters(): PredicateParameter[] {
        return this.parameters;
    }

    getTypeOfParam(param: Param): TypeTag | null {
        return this.funTag.getTypeOfParam(param);
    }

    // TODO: check and streamline this 
    paramTagsCoincide(other: TypeTag) {
        if (!isPredicateTTag(other)) return false;

        if (this.parameters.length !== other.parameters.length) return false;  
        for (let i = 0; i < this.parameters.length; i++) {
            if (!this.parameters[i].getType().sameTypeAs(other.parameters[i].getType())) return false;
        }
        return true;
    }

    sameTypeAs(other: TypeTag): boolean {
        return this.paramTagsCoincide(other);
    }
}

export class FunctionParameter {
    private readonly param: Param;
    private readonly type: TypeTag;

    constructor(param: Param, paramType: TypeTag) {
        this.param = param;
        this.type = paramType;
    }

    getParam() {
        return this.param;
    }

    /** Returns type of the param */
    getType() {
        return this.type;
    }

    toString() {
        return `${this.getParam().name}: ${this.getType().toString()}`;
    }
}

// export interface FunctionParameter {
//     param: Param;
//     type: TypeTag;
// }

export class PredicateParameter extends FunctionParameter {};

export function isFunctionTTag(tag: TypeTag): tag is FunctionTTag {
    return tag.tag === "Function";
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
        return `${this.tag}: ${this.sig.name}`;
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