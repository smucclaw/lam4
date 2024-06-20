grammar Lam4

/*
=========================
How to see this in action
==========================

Langium playground: https://langium.org/playground/
Go to the playground and paste the grammar in the editor to see the generated parser in action!

=======
TO DOs
=======
1. Think about whether to refactor this into 2 sub-grammars
https://langium.org/docs/recipes/multiple-languages/

============================================
Is it important to support mixfix operators?
============================================

I was initially worried about this.
But after thinking about it more and looking at a range of examples 
(Meng's and Joe's formalizations, various spreadsheet examples, etc), 
I don't feel as worried -- a lot of the uses of mixfix in the LE-targeted-formalizations
can be rephrased, and in any case, it doesn't seem as necessary in a more functional style.
It feels enough to support Haskell-like infix.
And it's also not clear that long mixfix'd expressions are that understandable to non-programmers anyway.
In particular, I'm not sure if a non-programmer is going to understand
what it really means without understanding enough of the underlying semantics.

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

================================================================

Currently experimenting with treating the type declarations along the lines of Alloy; in particular, treating data as relations
* https://github.com/tnelson/Forge/wiki/Sigs
* Daniel Jackson's book

Useful reference when writing this grammar: https://github.com/BestSolution-at/openapi-dsl

See also
    * https://json-schema.org/understanding-json-schema/reference
    * https://swagger.io/docs/specification/data-models/data-types/

==========================================
    Langium Notes
==========================================

    * Keywords can't include whitespace: https://github.com/eclipse-langium/langium/discussions/601
    * Avoid using newline as a delimiter: https://github.com/eclipse-langium/langium/discussions/1510
        > "I would generally recommend against employing newline-delimited languages, as they don't behave very well when it comes to error recovery behavior in Langium. Unless you're reimplementing an existing language in Langium, you're almost always better suited using a different delimiter like `;`. Generic parsers are generally pretty bad at handling unexpected input in whitespace-aware languages. It also makes maintenance on the grammar pretty difficult, as you always need to keep track where a newline (or multiple newlines) is required/optional/not allowed."

*/

entry Program:
    // (Types)?
    // (Rules)?
    ("GENERAL_METADATA_FOR_TYPES" gen_metadata=MetadataBlock)*
    ("GENERAL_METADATA_FOR_RULES" gen_metadata=MetadataBlock)*
    elements+=ToplevelElement*
;

// Types:
//     (types+=CustomType*)*
// ;

ToplevelElement:
    CustomType | Rule
;

// Rules:
//     ("GENERAL_METADATA_FOR_RULES" gen_metadata=MetadataBlock)*
//     (rules+=Rule)*
// ;


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

InlineMetadataBlock:
    '/-'
        properties+=MetadataKVPair+
    '-/'
;


/* ========================================
       Rules
======================================== */

// ------- Params -------
GivenParamBlock:
    'GIVEN' '('
        params+=ParamTypePair+
    ')'
;

Param:
    name=ID;

ParamTypePair:
    name=Param (':' | 'IS_A') paramType=TypeDef
;

// ------- PredicateDecl, FunDecl -------

/* TODO:
    FunDecl
    FunApp */

Rule:
    metadata=(InlineMetadataBlock)?
    rule=PredicateDecl 
        | FunDecl
;

// a terse FunDecl
FunDecl:
    // No GIVENs for this species of FunDecl -- having both the GIVEN syntax and the Typescripty params in parens syntax is too confusing
    'FUNCTION'? 
    name=ID '(' ( params+=Param (',' params+=Param)* )? ')' (':' returnType=TypeDef)?  
    // TODO: Switch Param to ParamTypePair once I figure out how to make the type annotations optional in the grammar
    (':=' | '=' | 'EQUALS') 
    body=Expr
    whereDecls=WhereBlock? 
    // TODO: Set linking rules to make it possible to link to vars tt are defined only in the where decls
    // See https://github.com/eclipse-langium/langium/discussions/1486
    // https://langium.org/docs/reference/document-lifecycle/
    // https://langium.org/docs/recipes/scoping/ 
    // https://langium.org/docs/recipes/scoping/class-member/
    // in the short term, might be easier to just use a LET
;

PredicateDecl:
    GivenParamBlock?
    'DECIDE' head=PredicateOrFuncID
    'IF' body=Expr 
    // IMPT: The 'IF' here is probably going to be a 'IFF' or 'just in case', rather than a mere 'if'
    whereDecls=WhereBlock?
;


// ------- Statements -------

WhereBlock:
    'WHERE' StatementBlock
;

StatementBlock:
    '{'
    statement+=Statement+
'}';

Statement:
    ThenIfStatement | VarDecl
;

ThenIfStatement infers Statement:
    then=VarDecl 'IF' condition=Expr
;

VarDecl returns NamedElement:
    {infer VarDecl} name=ID op=(':=' | '=' | 'IS_TO_BE') value=Expr
;


PredicateOrFuncID:
    {infer PredicateOrFuncID} value=(ID | BACK_TICKED_ID)
;


// ------- Expr -------

Expr:
    OrExpr
;

OrExpr infers Expr:
    AndExpr ({infer BinBoolOp.left=current} op='OR' right=AndExpr)*
;

AndExpr infers Expr:
    NumComparisonExpr ({infer BinBoolOp.left=current} op='AND' right=NumComparisonExpr)*
;


NumComparisonExpr infers Expr:
    Add ({infer ComparisonOp.left=current} op=('<' | '<=' | '>' | '>=' | 'EQUALS' | 'DIFFERS_FROM' | '==' | '!=') right=Add)*
;


Add infers Expr:
    Mult ({infer NumOp.left=current} op=('+' | '-') right=Mult)*
;

Mult infers Expr:
    PrimitiveExpr ({infer NumOp.left=current} op=('*' | '/') right=PrimitiveExpr)*
;

// TODO: Will prob want to refactor Ref into a MemberCall/Ref or something like that?
PrimitiveExpr infers Expr:
    '(' Expr ')' | UnaryExpr | Ref | NumberLiteral | BooleanLiteral
;

Ref:
    value=[NamedElement:ID];

UnaryExpr:
    op=('-' | 'NOT') value=Expr
;


type NamedElement = VarDecl | Param | ParamTypePair | PredicateDecl | SigDecl | Relation;


/* ========================================
       Sigs
======================================== */

// TODO: Add support for `SIG sig1, sig2, ..., sig_n SUBSET_OF OrigSig`
SigDecl:
    'DECLARE'? 'SIG' name=ID 
    ('(' descriminator=ID ('=' descriminatorValue=STRING)? ')')? 
    ( 'SUBSET_OF' parents+=[SigDecl:ID] ( ',' parents+=[SigDecl:ID] )* )? '{'
        relations+=Relation*
    '}'
;

CustomType:
    SigDecl 
    // | UnionType 
    // | Alias
;

Multiplicity returns string:
    "SET_OF" | "ONE" | "LONE"
;


Relation:
    metadata=(InlineMetadataBlock)?
    name=ID optional?='?'? 
    (':' | 'IS') 
    multiplicity=Multiplicity?
    relatum=TypeDef // TODO: Support plural relata 
    nullable?='?'? 
    description=SINGLELINE_METADATA_ANNOTATION?
;

TypeDef:
    (refType=[CustomType:ID] | builtinType=Builtin )
;


/* ========================================
         BUILT IN and PRIMITIVE types
 ======================================== */

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
terminal BACK_TICKED_ID:
    '`' -> '`'
;

// TODO: Figure out how to just drop the "--.."
// matches --
terminal SINGLELINE_METADATA_ANNOTATION: /--([^\n\r]*)/;

// matches start with /* ends with */
hidden terminal MULTILINE_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal SINGLELINE_COMMENT: /\/\/[^\n\r]*/;
