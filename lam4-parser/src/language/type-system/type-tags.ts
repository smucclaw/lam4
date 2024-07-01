import { AstNode } from "langium";
import {
    SigDecl,
    BooleanLiteral,
    StringLiteral,
    NumberLiteral,
    Param,
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
    readonly parameters: FunctionParameter[];
    constructor(parameters: FunctionParameter[], returnType: TypeTag) {
        this.returnType = returnType;
        this.parameters = parameters;
    }
    toString() {
        const params = this.parameters.map(p => `${p.param.name}: ${p.type.toString()}`).join(', ');
        return `(${params}) => ${this.returnType.toString()}`;
    }

    sameTypeAs(other: TypeTag): boolean {
        const self = this;
        function paramTagsCoincide(other: TypeTag) {
            if (!isFunctionTTag(other)) return false;
            if (self.parameters.length !== other.parameters.length) return false;  
            for (let i = 0; i < self.parameters.length; i++) {
                if (!self.parameters[i].type.sameTypeAs(other.parameters[i].type)) return false;
            }
            return true;
        }
        return paramTagsCoincide(other);
    }
}

export interface FunctionParameter {
    param: Param;
    type: TypeTag;
}

export function isFunctionTTag(tag: TypeTag): tag is FunctionTTag {
    return tag.tag === "Function";
}

/*============= Sig ================================ */

export class SigTTag implements TypeTag {
    readonly tag = "Sig";
    readonly sig: SigDecl;
    constructor(sig: SigDecl) {
        this.sig = sig;
    }
    toString() {
        return (this.sig as SigDecl).name;
    }

    sameTypeAs(other: TypeTag): boolean {
        return isSigTTag(other) && this.sig === other.sig;
    }
}

export function isSigTTag(tag: TypeTag): tag is SigTTag {
    return tag.tag === "Sig";
}


/* == Error type tag ================================== */

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

    sameTypeAs(other: TypeTag): boolean {
        return isErrorTypeTag(other) && this.astNode === other.astNode && this.message === other.message;
    }
}

export function isErrorTypeTag(tag: TypeTag): tag is ErrorTypeTag {
    return tag.tag === "TCError";
}