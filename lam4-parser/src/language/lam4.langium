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
* Finish supporting join / relational composition operator (which can also be thought of as a 'member/field dereferencing')
  by implementing the analogue of https://langium.org/docs/recipes/scoping/class-member/
* May need to use TokenBuilder or smtg like that to support "'s" --- right now the ast.ts seems to use single quotes for 
* Check if adding LetExpr to the grammar in way I have is compositional enough
* Think about whether to refactor this into 2 sub-grammars
https://langium.org/docs/recipes/multiple-languages/
* Think about what a more user-friendly name for `SIG` might be -- perhaps `CONCEPT`?



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
    ("GENERAL_METADATA_FOR_TYPES" gen_metadata=GeneralMetadataBlock)*
    ("GENERAL_METADATA_FOR_RULES" gen_metadata=GeneralMetadataBlock)*
    elements+=ToplevelElement*
;

ToplevelElement:
    Facts | SigDecl | PredicateDecl | FunDecl
;

/* ========================================
       Metadata
======================================== */

GeneralMetadataBlock:
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
    GeneralMetadataBlock | NumberLiteral | StringLiteral | BooleanLiteral
;

SpecificMetadataBlock:
    '/-'
        properties+=MetadataKVPair+
    '-/'
;

/* ========================================
       Facts
======================================== */

// these would be the global vars (usually constants). These would prob just be sugar for a `let...in`
// in the future, would want to think about how to support things like Representations and Warranties (which go beyond mere statements of facts)
Facts:
    'FACTS' '{'
        facts+=VarDecl+
    '}'
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
    name=ID (':' | 'IS_A') paramType=TypeDef
;

// ------- PredicateDecl, FunDecl -------

fragment PotentiallyWithSpecificMetadataBlock:
    metadata=(SpecificMetadataBlock)?
;

// a terse FunDecl
FunDecl:
    PotentiallyWithSpecificMetadataBlock

    // No GIVENs for this species of FunDecl -- having both the GIVEN syntax and the Typescripty params in parens syntax is too confusing
    'FUNCTION'? 
    name=ID '(' ( params+=Param (',' params+=Param)* )? ')' (':' returnType=TypeDef)?  
    // TODO: Switch Param to ParamTypePair once I figure out how to make the type annotations optional in the grammar
    (':=' | '=' | 'EQUALS') 
    body=Expr
    // whereDecls=WhereBlock? 
    // TODO: Set linking rules to make it possible to link to vars tt are defined only in the where decls
    // See https://github.com/eclipse-langium/langium/discussions/1486
    // https://langium.org/docs/reference/document-lifecycle/
    // https://langium.org/docs/recipes/scoping/ 
    // https://langium.org/docs/recipes/scoping/class-member/
    // in the short term, might be easier to just use a LET
;

PredicateDecl:
    PotentiallyWithSpecificMetadataBlock

    GivenParamBlock?
    'DECIDE' name=PredicateOrFuncID 
    // can think of 'name' as 'head'
    'IF' body=Expr 
    // IMPT: The 'IF' here is probably going to be a 'IFF' or 'just in case', rather than a mere 'if'
    // whereDecls=WhereBlock?
;


// ------- Statements -------

// WhereBlock:
//     'WHERE' StatementBlock
// ;

// StatementBlock:
//     '{'
//     statement+=Statement+
// '}';

// Statement:
//     ThenIfStatement | VarDecl
// ;

// ThenIfStatement infers Statement:
//     then=VarDecl 'IF' condition=Expr
// ;

VarDecl returns NamedElement:
    {infer VarDecl} name=ID op=(':=' | '=' | 'IS_TO_BE') value=Expr description=SINGLELINE_METADATA_ANNOTATION?
;


PredicateOrFuncID:
    {infer PredicateOrFuncID} value=(ID | BACK_TICKED_ID)
;


// ------- Expr -------

Expr:
    OrExpr | LetExpr
    // TODO: Check if adding LetExpr to the grammar in this way is compositional enough
;

OrExpr infers Expr:
    AndExpr ({infer BinBoolOp.left=current} op='OR' right=AndExpr)*
;

AndExpr infers Expr:
    (NumComparisonExpr| IfThenElseExpr) ({infer BinBoolOp.left=current} op='AND' right=(NumComparisonExpr| IfThenElseExpr))*
;

LetExpr infers Expr:
    'LET' '{' (vars+=VarDecl)+ '}' 'IN' '{' body=OrExpr '}'
;

IfThenElseExpr infers Expr:
    'IF' condition=OrExpr 'THEN' then=OrExpr 'ELSE' else=OrExpr
;

NumComparisonExpr infers Expr:
    Add ({infer ComparisonOp.left=current} op=('<' | '<=' | '>' | '>=' | 'EQUALS' | 'DIFFERS_FROM' | '==' ) right=Add)*
;


Add infers Expr:
    Mult ({infer NumOp.left=current} op=('+' | '-') right=Mult)*
;

Mult infers Expr:
    RelationalJoin ({infer NumOp.left=current} op=('*' | '/') right=RelationalJoin)*
;

/*
ISSUE: Linking to 'fields' will require implementing something like https://langium.org/docs/recipes/scoping/class-member/ --- in particular, this seems to involve adding some kind of type system? Not clear now that it's worth using Langium.

(To be clear, the auto-linking/crossref for this won't work in the playground: https://github.com/eclipse-langium/langium/discussions/1502)

The relationship between relational join and a more conventional member access operator:
    Can think of dereferencing a field of an object as corresponding to a special case of relational join --- this is a common trick / idiom in the Alloy community
*/
RelationalJoin infers Expr:
    PrimitiveExpr ({infer RelationalJoin.left=current} op=("`s") right=PrimitiveExpr)*
    // TODO: Temporarily using `s instead of 's because I'm worried the single quote might cause issues (they use single quotes when generating the AST). 
    // Can prob allow for "'s" by customizing the TokenBuilder
;

FunctionApplication infers Expr:
    func=[NamedElement:ID]
    ('(' args+=Expr (',' args+=Expr)* ')')
;

PrimitiveExpr infers Expr:
    '(' Expr ')' | FunctionApplication  | Ref | UnaryExpr | NumberLiteral | BooleanLiteral
;

Ref:
    value=[NamedElement:ID];

UnaryExpr:
    op=('-' | 'NOT') value=Expr
;


/* ========================================
       Sigs
======================================== */

/* The idea I had been playing with was to express structure with (flat / first-order) relations, a la Alloy.

The idea is this. Given something like

SIG A {
    myField: B
}

each field `f` would be translated to a relation with arity 1 + arity(f).

E.g., the above sig would be translated to a relation named myField of type A -> B.

And then the `'s` operator would correspond to relational join / composition.

Why do this? The rough motivation was, because, 
    1. I wanted to have, not only a functional expression language, but also the ability to do some kind of formal modelling or querying on top of that
    2. and as Alloy and its ilk have shown, 
    modelling data with relations and adding a relational-logic constraint language 
    (the quantifiers of first-order logic with the operators of the relational calculus) 
    on top of that gives you a nice way to formally model a lot of things.
    (See Daniel Jackson's book *Software Abstractions* for more on this.)
    And we can think of something like a conventional OOP '<object>.<field>` as being a special case of relational join,
    if we think of a sig as corresponding to an OOP 'class'.

(Nor is expressing data with relations unique even among legal DSLs -- e.g., Eiger does this.)

That said, I'm *not* sure if this really is the way to handle data --- it just felt like a reasonable choice given that I also wanted to have some support for formal modelling, and given my past experience with Alloy.
*/
// TODO: Add support for `SIG sig1, sig2, ..., sig_n SUBSET_OF OrigSig`
SigDecl:
    'DECLARE'? 'SIG' 
    name=ID 
    ( 'SUBSET_OF' parents+=[SigDecl:ID] ( ',' parents+=[SigDecl:ID] )* )? '{'
        relations+=Relation*
    '}'
;

type CustomType = SigDecl;
// CustomType:
//     SigDecl 
// | UnionType 
// | Alias
// ;

/* The idea of 'Multiplicity' is from Alloy:
    set of: a set of values (i.e., this field may contain any number of objects);
    one: a singleton value (this field always contains a single object); and
    lone: either a singleton value or no value (this field contains 0 or 1 object).
*/
Multiplicity returns string:
    "SET_OF" | "ONE" | "LONE"
;


Relation:
    PotentiallyWithSpecificMetadataBlock

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

// type Rule = FunDecl | PredicateDecl;
type NamedElement = VarDecl | Param | ParamTypePair | PredicateDecl | FunDecl  | Relation | SigDecl;


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

BuiltinBoolFormat returns string:
    "Boolean"
;

Builtin returns string:
    BuiltinStringFormats | BuiltinBoolFormat | BuiltinNumberFormats | BuiltinDateTimeRelated
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
    value='True' | 'False'
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
