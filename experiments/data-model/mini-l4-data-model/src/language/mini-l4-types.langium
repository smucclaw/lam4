grammar MiniL4Types

/*
Langium playground: https://langium.org/playground/
Go to the playground and paste the grammar in the editor to see the generated parser in action!

Useful reference when writing this grammar: https://github.com/BestSolution-at/openapi-dsl

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
    (array?='array' '<' typeDef=TypeDef (',' maxItems=INT)? '>') | typeDef=TypeDef
;

TypeDef:
    (refType=[CustomType:ID] | builtinType=Builtin )
;


// /* ========================================
//         BUILT INs and PRIMITIVEs
// ======================================== */

// TODO: Go through these more carefully; remove the ones we don't need or won't be able to support in the very short term

// Sticking to the numeric types supported by json schema for now
// Ideally we would start by thinking about what types *we* want to have in L4, independently of what's available in JSON Schema,
// and just warn the user if they want to produce a JSON Schema from types that are incompatible / not available in JSON Schema. 
// But in the short term, it's easier if we limit ourselves to the the JSON Schema types.
// https://json-schema.org/understanding-json-schema/reference/numeric
BuiltinNumberFormats returns string:
    'Integer' | 'IntegerOrFloat'
;

BuiltinStringFormats returns string:
    'String'
;

// https://json-schema.org/understanding-json-schema/reference/string
BuiltinDateTimeRelated returns string:
    'Date' | 'DateTime' | 'Time' | 'Duration'
;

// in the future: BuiltinMoneyFormats ?
Builtin returns string:
    BuiltinStringFormats | BuiltinNumberFormats | BuiltinDateTimeRelated;


// Could try to support specific currencies in the future
// BuiltinMoneyFormats returns string:
//     'Money'
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

// matches --
terminal SINGLELINE_METADATA_ANNOTATION: /-- [^\n\r]*/;

// matches start with /* ends with */
hidden terminal MULTILINE_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal SINGLELINE_COMMENT: /\/\/[^\n\r]*/;