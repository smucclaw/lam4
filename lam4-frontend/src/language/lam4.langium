grammar Lam4

/*
=========================
How to see this in action
==========================

If you don't need to use record field access or predicate declarations:
    you can paste the grammar into the playground (https://langium.org/playground/) to see the generated parser in action!

If you do need either of those:
   1. `npm run langium:generate;  npm run build`
   2. Then either print to console the resulting AST with `node ./bin/cli printAstForHumans <name of .l4 file>` 
   or build a VSCode extension with `vsce package --allow-missing-repository
` and install it from VSCode

=======
TO DOs
=======
* Look into how to enable language server support for non-VSCode editors
* Test the type checking / inference and scoping more
* Check if adding LetExpr to the grammar in way I have is compositional enough
* Think about whether to refactor this into 2 sub-grammars
https://langium.org/docs/recipes/multiple-languages/
* Check if the handling of number literals is good enough


after first draft of bidir type checker is done
* Add Let, anon func to the type checker
* Add Fractions back in 
* Implement source maps functionality: https://www.typefox.io/blog/code-generation-for-langium-based-dsls-3/



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
    // ("GENERAL_METADATA_FOR_TYPES" gen_metadata=GeneralMetadataBlock)*
    // ("GENERAL_METADATA_FOR_RULES" gen_metadata=GeneralMetadataBlock)*
    elements+=ToplevelElement*
;

ToplevelElement:
    SigDecl | PredicateDecl | FunDecl
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
    name=MetadataKey ':' value=MetadataValue
;

MetadataValue:
    GeneralMetadataBlock | ListOfMetadataValues | Literal
;

ListOfMetadataValues:
    '[' value+=Literal 
        (',' value+=Literal)* ']'
;

SpecificMetadataBlock:
    '/-'
        properties+=MetadataKVPair+
    '-/'
;

/* ========================================
       Rules
======================================== */

// ------- Params -------
fragment GivenParamBlock:
    'GIVEN' '('
        params+=ParamTypePair*
    ')' 
;

Param:
    name=ID;

ParamTypePair:
    param=Param (':' | 'IS_A') type=TypeAnnot
;

// ------- PredicateDecl, FunDecl -------

fragment WithSpecificMetadataBlock:
    metadata=SpecificMetadataBlock
;

// TODO: Figure out how best to add support for clauses -- i.e. for things like §10(a) or §10(1)(a)
OriginalRuleRef:
    ('SECTION' | '§') section=INTEGER
;

// a terse FunDecl
FunDecl:
    originalRuleRef=OriginalRuleRef?
    WithSpecificMetadataBlock?

    // No GIVENs for this species of FunDecl -- having both the GIVEN syntax and the Typescripty params in parens syntax is too confusing
    // Require annotations for top level func decl, since required for bidir type checking
    funType=TypeAnnot

    'FUNCTION'? // An explicit kw might be helpful for non-programmers
    name=ID '(' ( params+=Param (',' params+=Param)* )? ')'
    ( '=' | 'EQUALS') 
    body=Expr
;

// When doing concrete (as opposed to symbolic) execution, 
// a predicate amounts to a special case of a function; 
// namely, a function whose return type is Boolean
PredicateDecl:
    originalRuleRef=OriginalRuleRef?
    
    WithSpecificMetadataBlock?

    GivenParamBlock
    'DECIDE' name=IDOrBackTickedID 
    // can think of 'name' as 'head'
    'IF' body=Expr 
    /* The 'IF' here is more like a 'IFF' or 'just in case', rather than a mere 'if',
    since "the law implicitly assumes a condition is false unless specified otherwise" 
    (https://catala-lang.org/en/examples/tutorial)
    In this way this is similar to Catala's 'Rule' */
;


VarDecl returns NamedElement:
    {infer VarDecl} name=ID (':' varType=TypeAnnot)? ( '=' | 'IS_TO_BE') value=Expr 
    // description=SINGLELINE_METADATA_ANNOTATION?
;


IDOrBackTickedID returns string:
    ID | BACK_TICKED_ID
;


// ------- Expr -------

Expr:
    OrExpr | LetExpr
    // TODO: Check if adding LetExpr to the grammar in this way is compositional enough
;

OpOr: {infer OpOr} 'OR';
OrExpr infers Expr:
    AndExpr ({infer BinExpr.left=current} op=OpOr right=AndExpr)*
;

OpAnd: {infer OpAnd} 'AND';
AndExpr infers Expr:
    (ComparisonExpr| IfThenElseExpr) ({infer BinExpr.left=current} op=OpAnd right=(ComparisonExpr| IfThenElseExpr))*
;

// TODO: Not sure if VarDecl the best thing to use for the inner stuff
LetExpr:
    ('LET' | 'DEFINE') '{' (vars+=VarDecl)+ '}' 'IN' '{' body=OrExpr '}'
;

IfThenElseExpr:
    'IF' condition=OrExpr 'THEN' then=OrExpr 'ELSE' else=OrExpr
;


OpLt: {infer OpLt} '<' ;
OpLte: {infer OpLte}'<=' ;
OpGt: {infer OpGt} '>';
OpGte: {infer OpGte} '>=';
OpEquals: {infer OpEquals} 'EQUALS' | '==';
OpNotEquals: {infer OpNotEquals} "DIFFERS_FROM" | "!=";
ComparisonOp: OpLt | OpLte | OpGt | OpGte | OpEquals | OpNotEquals;

ComparisonExpr infers Expr:
    Plus ({infer BinExpr.left=current} op=ComparisonOp right=Plus)*
;

OpPlus: {infer OpPlus} '+';
OpMinus: {infer OpMinus} '-';
OpNot: {infer OpNot} 'NOT';
Plus infers Expr:
    Mult ({infer BinExpr.left=current} op=(OpPlus | OpMinus) right=Mult)*
;

OpMult: {infer OpMult} '*';
OpDiv: {infer OpDiv} '/';
Mult infers Expr:
    Join ({infer BinExpr.left=current} op=(OpMult | OpDiv) right=Join)*
;

/*
TODO: Add a variant of this in the standard library for relational join in the other direction --- maybe use "`of`"

(To be clear, the auto-linking/crossref for this won't work in the playground: https://github.com/eclipse-langium/langium/discussions/1502)

The relationship between relational join and a more conventional member access operator:
    Can think of dereferencing a field of an object as corresponding to a special case of relational join --- this is a common trick / idiom in the Alloy community
*/
Join infers Expr:
    FunctionApplication ({infer Join.left=current} '`s`' right=Ref)*
    // Right can't be [NamedElement:ID]
    // because we want the scoper to visit `left` first,
    // so that if `left`'s reference will be resolved via the default scoper/linker, if that's possible.
    // This allows us to then use that resolved reference
    // as a base case in figuring out the reference for `right`

    // TODO: 's and `s don't work out of the box
    // Can prob allow for "'s" by customizing the TokenBuilder
;

// with postfix predicate application as a special case of this, when doing *concrete* evaluation
FunctionApplication:
    PrimitiveExpr
    ({infer FunctionApplication.func=current}
    ('(' args+=Expr (',' args+=Expr)* ')'))*
;

// sugar for func app, when doing *concrete* evaluation
InfixPredicateApplication: 
   ( args+=[NamedElement:ID] 'IS'? predicate=Expr ) // TODO: not sure actually that we even want to have `IS` as an option -- might be better style to have that in the name of the predicate itself
   | ( '(' args+=Expr (',' args+=Expr)* ')' ('IS' | 'ARE')? predicate=Expr )
;

// This is meant for programmers to implement libraries with; 
// it's not something I'm expecting ordinary, non-programmer users to use or understand
AnonFunction:
    '\\'  
    ( ('(' ')') | (params+=(ParamTypePair | Param) (params+=(ParamTypePair | Param))*) ) 
    '=>' 
    body=Expr
;

PrimitiveExpr infers Expr:
    '(' Expr ')' | AnonFunction | InfixPredicateApplication | Ref | UnaryExpr | IntegerLiteral | BooleanLiteral
    // Not sure if we want to allow for string literals as expressions
;

Ref:
    value=[NamedElement:IDOrBackTickedID];

UnaryExpr:
    op=(OpMinus | OpNot) value=Expr
;


/* ========================================
       Sigs / Concepts
======================================== */

/* 
For the most part, especially when just evaluating,
can think of Concepts as records.

There's also a good chance I'll just give up on the Alloy stuff.

------------

The idea I had been playing with was to express structure with (flat / first-order) relations, a la Alloy.

The idea is this. Given something like

CONCEPT A {
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
    3. I haven't seen any real need for objects --- in particular, for the sort of procedural abstraction they provide --- when modelling / formalizing regulations

(Nor is expressing data with relations unique even among legal DSLs -- e.g., Eiger does this.)

That said, I'm *not* sure if this really is the way to handle data --- it just felt like a reasonable choice given that I also wanted to have some support for formal modelling, and given my past experience with Alloy.
*/
// TODO: Add support for `CONCEPT sig1, sig2, ..., sig_n SPECIALIZES OrigSig`
SigDecl:
    WithSpecificMetadataBlock?

    'DECLARE'? 'CONCEPT' 
    name=ID 
    ( 'SPECIALIZES' parents+=[SigDecl:ID] ( ',' parents+=[SigDecl:ID] )* )? '{'
        relations+=Relation*
    '}'
;

type CustomType = SigDecl;
// CustomType:
//     SigDecl 
// | UnionType 
// | Alias
// ;


Relation:
    WithSpecificMetadataBlock?

    name=ID 
    // optional?='?'? 
    (':' | 'IS_A') 
    relatum=TypeAnnot // TODO: Support plural relata 
    description=SINGLELINE_METADATA_ANNOTATION?
;

// Not expecting non-programmers to understand the more complicated type annotations in the textual form
// This would be visualized / rendered in more accessible ways for non-programmers
TypeAnnot:
    // `=>` is right-associative (c.f. https://www.typefox.io/blog/parsing-expressions-with-xtext/)
    PrimitiveTypeAnnot ({infer TypeAnnot.left=current} '=>' right=TypeAnnot)?
;

PrimitiveTypeAnnot:
    CustomTypeDef | BuiltinType | '(' TypeAnnot ')'
;

CustomTypeDef:
    annot = [CustomType:ID]
;

type NamedElement = VarDecl | Param | PredicateDecl | FunDecl  | Relation | SigDecl | Relation;

/* ========================================
         BUILT IN and PRIMITIVE types
 ======================================== */

BuiltinType:
    annot = BuiltinTypeAnnot
;

// TODO: Think about whether to add DateTime, Time and Duration (days)
// https://json-schema.org/understanding-json-schema/reference/string
BuiltinTypeAnnot returns string:
     'Integer' | 'String' |  'Boolean' | 'Unit'
    //  'Fraction' 
    //  'Date'
;


/* ========================================
        LITERALS
======================================== */

Literal: IntegerLiteral | StringLiteral | BooleanLiteral;

IntegerLiteral:
    value=INTEGER
;

StringLiteral:
    value=STRING
;

BooleanLiteral:
    value=('True' | 'False')
;

// Decimal returns string:
//     INT ('.' INT)?;


/* ========================================
        TERMINALS
======================================== */

hidden terminal WS: /\s+/;
terminal ID: /[_a-zA-Z][\w_]*/;
terminal INTEGER returns string: /((-|\\+)?[0-9]+)/;
// terminal NUMBER returns number: /[0-9]+(\.[0-9]+)?/;
// terminal SIGNED_NUMBER returns number: /-\d+((\.\d+)?([eE][\-+]?\d+)?)?/;
// terminal UNSIGNED_NUMBER returns string: /\d+((\.\d+)?([eE][\-+]?\d+)?)?/;
terminal STRING: /"(\\.|[^"\\])*"|'(\\.|[^'\\])*'/;
terminal BACK_TICKED_ID:
    '`' -> '`'
;

// matches --
terminal SINGLELINE_METADATA_ANNOTATION: /--([^\n\r]*)/;

// matches start with /* ends with */
hidden terminal MULTILINE_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal SINGLELINE_COMMENT: /\/\/[^\n\r]*/;
