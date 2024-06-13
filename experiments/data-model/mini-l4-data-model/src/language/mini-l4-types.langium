grammar MiniL4Types

/*
Langium playground: https://langium.org/playground/
Go to the playground and paste the grammar in the editor to see the generated parser in action!

====================
What built-in types?
====================

Jun 13 2024
-----------
    Andres:
        I think we need at least:
            strings,
            numbers (at least integers, probably also some form of fractional numbers, 
            but possibly rational numbers are sufficient; it'd be good to avoid IEEE floats completely),
            lists (or possibly another collection type)
        Less clear are:
            dates / times / durations,
            commodities / money,
            sets,
            dictionaries (possibly subsumed by being to define user-defined record types)

    YM: 
        * Added Fraction as per the above (though might be more user-friendly to call it 'Ratio'?)
        * Added dates since those were needed for the insurance web form. 
            Could think of dates as being in a library instead of the core language, indeed
            that might be cleaner, but it's more convenient for now to have them as a built-in type.
        * Shelving `Money` for the time being

=========================================================================================

Useful reference when writing this grammar: https://github.com/BestSolution-at/openapi-dsl

See also
    * https://json-schema.org/understanding-json-schema/reference
    * https://swagger.io/docs/specification/data-models/data-types/

Note:
    * Keywords can't include whitespace: https://github.com/eclipse-langium/langium/discussions/601
*/

entry Model:
    ('GENERAL_METADATA_FOR_TYPES' gen_metadata=MetadataBlock)*
    (types+=CustomType*)*
;

/* ========================================
       Metadata
======================================== */

MetadataBlock:
    '{'
        properties+=MetadataKVPair+
    '}'
;
MetadataKey returns string:
    ID | "description" | "description_for_semantic_parser"
;

MetadataKVPair:
    name=MetadataKey ':' (value=MetadataValue | '[' values+=MetadataValue (',' values+=MetadataValue)* ']')
;

MetadataValue:
    MetadataBlock | NumberLiteral | StringLiteral | BooleanLiteral
;

MetadataForProperty:
    '/-'
        properties+=MetadataKVPair+
    '-/'
;

ValueType:
    'DECLARE' name=ID ('(' descriminator=ID ('=' descriminatorValue=STRING)? ')')? ( 'EXTENDS' parents+=[ValueType:ID] ( ',' parents+=[ValueType:ID] )* )? '{'
        properties+=Property*
    '}'
;

UnionType:
    'UNION' name=ID '=' types+=[ValueType:ID] ( '|' types+=[ValueType:ID] )+
;

Alias:
    'ALIAS' name=ID '=' type=Type
;

CustomType:
    ValueType | UnionType | Alias;

Property:
    metadata=(MetadataForProperty)?
    name=ID optional?='?'? (':' | 'IS_A' | 'IS_THE') type=Type nullable?='?'? description=SINGLELINE_METADATA_ANNOTATION?
;

Type:
    (list?='LIST' '<' typeDef=TypeDef (',' maxItems=INT)? '>') | typeDef=TypeDef
;

TypeDef:
    (refType=[CustomType:ID] | builtinType=Builtin )
;


// /* ========================================
//         BUILT INs and PRIMITIVEs
// ======================================== */

// TODO: Go through these more carefully; remove the ones we don't need or won't be able to support in the very short term

BuiltinNumberFormats returns string:
    'Integer' | 'Fraction'
;

BuiltinStringFormats returns string:
    'String'
;

// TODO: Think about whether to add DateTime, Time and Duration (days)
// https://json-schema.org/understanding-json-schema/reference/string
BuiltinDateTimeRelated returns string:
    'Date'
;

Builtin returns string:
    BuiltinStringFormats | BuiltinNumberFormats | BuiltinDateTimeRelated
;

// Shelving Money for now, but let's revisit this soon
// TODO: Factor out the currencies
// BuiltinMoneyFormats returns string:
//     'Money(USDCents)' | 'Money(SGDCents)' | 'Money(Other)'
// ;

// If you were trying to model OpenAPI, you would use the following too:
// BuiltinIntegerFormats returns string:
//     'int32' | 'int64'
// ;
// BuiltinFloatFormats returns string:
//     'float'
// ;
// PrimitiveString:
//     type='string' '(' format=(ID|BuiltinStringFormats) ')'
// ;

// Primitive:
//     PrimitiveString | PrimitiveFloats | PrimitiveIntegers;

// PrimitiveFloats:
//     'number' '(' format=(ID|BuiltinFloatFormats) ')' 
//         (lowerBound=('('|'[') lower=INT? ',' upper=INT? upperBound=(')'|']'))?
//         ('mul' multipleOf=UNSIGNED_NUMBER)?
// ;

// PrimitiveIntegers:
//     'integer' '(' format=(ID|BuiltinIntegerFormats) ')' 
//         (lowerBound=('('|'[') lower=INT? ',' upper=INT? upperBound=(')'|']'))?
//         ('mul' multipleOf=UNSIGNED_NUMBER)?
// ;

/* ========================================
        LITERALS
======================================== */

// TODO: Clean number literals / signed vs unsigned num up
Decimal returns number:
    INT ('.' INT)?;

NumberLiteral:
    value=SIGNED_NUMBER|UNSIGNED_NUMBER|INT|Decimal
;

StringLiteral:
    value=STRING
;

BooleanLiteral:
    value='true' | 'false'
;

/* ========================================
        TERMINALS
======================================== */

hidden terminal WS: /\s+/;
terminal ID: /[_a-zA-Z][\w_]*/;
terminal INT returns number: /[0-9]+/;
terminal SIGNED_NUMBER returns number: /-\d+((\.\d+)?([eE][\-+]?\d+)?)?/;
terminal UNSIGNED_NUMBER returns number: /\d+((\.\d+)?([eE][\-+]?\d+)?)?/;
terminal STRING: /"(\\.|[^"\\])*"|'(\\.|[^'\\])*'/;

// TODO: Figure out how to just drop the "--.."
// matches --
terminal SINGLELINE_METADATA_ANNOTATION: /-- [^\n\r]*/;

// matches start with /* ends with */
hidden terminal MULTILINE_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal SINGLELINE_COMMENT: /\/\/[^\n\r]*/;