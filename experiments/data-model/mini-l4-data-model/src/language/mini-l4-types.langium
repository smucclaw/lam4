grammar MiniL4Types

/*
Langium playground: https://langium.org/playground/
Go to the playground and paste the grammar in the editor to see the generated parser in action!

Useful reference when writing this grammar: https://github.com/BestSolution-at/openapi-dsl

Note:
    * Keywords can't include whitespace: https://github.com/eclipse-langium/langium/discussions/601
*/

entry Model:
    ('GENERAL_METADATA' gen_metadata=MetadataBlock)*
    ('TYPES' '{' types+=CustomType* '}')?
;

/* ========================================
       Metadata
======================================== */

MetadataBlock:
    '{'
        properties+=MetadataKVPair+
    '}'
;

MetadataKVPair:
    name=ID ':' (value=MetadataValue | '[' values+=MetadataValue (',' values+=MetadataValue)* ']')
;

MetadataValue:
    MetadataBlock | NumberLiteral | StringLiteral | BooleanLiteral
;

MetadataForProperty:
    '/-'
        properties+=MetadataKVPair+
    '-/'
;

// ModelValue:
//     ModelType | StringLiteral
// ;

// ModelType:
//     '{'
//         properties+=ModelProperty+
//     '}'
// ;
// ModelProperty:
//     name=ID ':' (value=ModelValue | '[' values+=ModelValue (',' values+=ModelValue)* ']')
// ;

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
    (refType=[CustomType:ID] | builtinType=Builtin | primitive=Primitive )
;


/* ========================================
        BUILT INs and PRIMITIVEs
======================================== */

BuiltinIntegerFormats returns string:
    'int32' | 'int64'
;

BuiltinFloatFormats returns string:
    'float' | 'double'
;

BuiltinStringFormats returns string:
    'String' | 'Money' | 'Password' | 'Date' | 'Date-Time'
;

Builtin returns string:
    BuiltinStringFormats | BuiltinIntegerFormats | BuiltinFloatFormats;

PrimitiveString:
    type='string' '(' format=(ID|BuiltinStringFormats) ')'
;

Primitive:
    PrimitiveString | PrimitiveFloats | PrimitiveIntegers;

PrimitiveFloats:
    'number' '(' format=(ID|BuiltinFloatFormats) ')' 
        (lowerBound=('('|'[') lower=INT? ',' upper=INT? upperBound=(')'|']'))?
        ('mul' multipleOf=UNSIGNED_NUMBER)?
;


PrimitiveIntegers:
    'integer' '(' format=(ID|BuiltinIntegerFormats) ')' 
        (lowerBound=('('|'[') lower=INT? ',' upper=INT? upperBound=(')'|']'))?
        ('mul' multipleOf=UNSIGNED_NUMBER)?
;

/* ========================================
        LITERALS
======================================== */

// TODO: Clean number literals up
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


// /\/\-[\s\S]*?\-\//;
// matches --
terminal SINGLELINE_METADATA_ANNOTATION: /-- [^\n\r]*/;

// matches start with /* ends with */
hidden terminal MULTILINE_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal SINGLELINE_COMMENT: /\/\/[^\n\r]*/;