grammar Lam4

/*
=================
Guiding Principle
=================
    * Keep the syntax relatively simple and constrained, while still being readable
    * Use UI/UX, AI, program synthesis, formal methods etc to make it easy for non-technical users to understand and program in or do things with Lam4

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
* think about whether it's clear how to conjoin IfThenOtherwise statements
* [MAYBE] add sugar along the lines of the GF group's `each of` (sugar for nested`AND`)
* Look into how to enable language server support for non-VSCode editors
* Test the type checking / inference and scoping more
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
    RecordDecl | SigDecl | PredicateDecl | FunDecl | ToplevelStatement | ReportDecl
;

/* ========================================
       Metadata
======================================== */

// GeneralMetadataBlock:
//     '{'
//         properties+=MetadataKVPair+
//     '}'
// ;
// MetadataKey returns string:
//     ID | "description"
//     // | "description_for_semantic_parser"
// ;

// MetadataKVPair:
//     name=MetadataKey ':' value=MetadataValue
// ;

// MetadataValue:
//     GeneralMetadataBlock | ListOfMetadataValues | Literal
// ;

// ListOfMetadataValues:
//     '[' value+=Literal
//         (',' value+=Literal)* ']'
// ;


// Sep 2024 -- Limiting the metadata keys / don't allow for arbitrary metadata key-value pairs in the short term.
SpecificMetadataBlock:
    '/-'
        // 'About' instead of 'about' b/c that feels more in line with legal style
        // TODO: not sure if we want the delimiters to be double quotes
        ("About:" description=StringLiteral)?

        // TODO: Add constructs for annotating / adjusting what the rendered explanations look like
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
    name=IDOrBackTickedID;

ParamTypePair:
    param=Param (':' | 'IS_A') type=TypeAnnot
;

// ------- PredicateDecl, FunDecl -------

// Sep 2024: Now trying to be more selective about where this sort of key-value metadata can be inserted,
// because I'm not sure that a key-value syntax is really the best for this,
// because this simplifies some of the downstream parsing,
// and because it feels safer to be more selective till we are sure we need it
fragment WithSpecificMetadataBlock:
    metadata=SpecificMetadataBlock
;

// TODO: Figure out how best to add support for clauses -- i.e. for things like §10(a) or §10(1)(a)
OriginalRuleRef:
    ('SECTION' | '§') section=INTEGER
;

fragment FunParams:
    '(' ( params+=Param (',' params+=Param)* )? ')'
;

fragment AnonFunParams:
    ( params+=Param ( params+=Param)* ) | '(' ')'
;


// a terse FunDecl
FunDecl:
    description=SINGLELINE_METADATA_ANNOTATION?
    originalRuleRef=OriginalRuleRef?
    // WithSpecificMetadataBlock?

    // No GIVENs for this species of FunDecl -- having both the GIVEN syntax and the Typescripty params in parens syntax is too confusing
    // Require annotations for top level func decl, since required for bidir type checking
    'FUNCTION' // An explicit kw might be helpful for non-programmers
    funType=TypeAnnot
    name=IDOrBackTickedID 
    FunParams
    ( '=' | 'equals')
    body=Expr
    'END'
;

// When doing concrete (as opposed to symbolic) execution,
// a predicate amounts to a special case of a function;
// namely, a function whose return type is Boolean
PredicateDecl:
    description=SINGLELINE_METADATA_ANNOTATION?
    originalRuleRef=OriginalRuleRef?
    // WithSpecificMetadataBlock?

    GivenParamBlock?
    'DECIDE' left=[Param:IDOrBackTickedID]? name=IDOrBackTickedID right=[Param:IDOrBackTickedID]?
    // can think of 'name' as 'head'
    'IF' body=Expr
    /* The 'IF' here is more like a 'IFF' or 'just in case', rather than a mere 'if',
    since "the law implicitly assumes a condition is false unless specified otherwise"
    (https://catala-lang.org/en/examples/tutorial)
    In this way this is similar to Catala's 'Rule' */
;

fragment Var:
    name=IDOrBackTickedID (':' varType=TypeAnnot)? ( '=' | 'is') value=Expr
;

VarBinding returns NamedElement:
    {infer VarBinding} Var
;


IDOrBackTickedID returns string:
    ID | BACK_TICKED_ID
;


/* ============================
    Expr
=============================== */

Expr:
    OrExpr | RecordExpr
    // TODO: Check if adding LetExpr to the grammar in this way is compositional enough
;

OpOr: {infer OpOr} 'OR';
OrExpr infers Expr:
    AndExpr ({infer BinExpr.left=current} op=OpOr right=AndExpr)*
;

// TODO: Would be good to add sugar along the lines of `each of`
OpAnd: {infer OpAnd} 'AND';
AndExpr infers Expr:
    (ComparisonExpr| IfThenElseExpr) ({infer BinExpr.left=current} op=OpAnd right=(ComparisonExpr| IfThenElseExpr))*
;

// TODO: Not sure if VarDecl the best thing to use for the inner stuff
// TODO: Will switch from braces to indentation when time permits
LetExpr:
    'LET' 
        '{' 
            vars+=VarBinding (',' vars+=VarBinding )* 
        '}' 'IN' '{' 
            body=Expr 
        '}'
;

Row:
    label=IDOrBackTickedID ( '=' | 'is') value=Expr
;

RecordExpr:
    '{|' (rows+=Row  (',' rows+=Row )*)? '|}'
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
    Project ({infer BinExpr.left=current} op=(OpMult | OpDiv) right=Project)*
;

// TODO: 's and `s don't work out of the box
// Can prob allow for "'s" by customizing the TokenBuilder
/* Projects or Selects the value of a from a record

(To be clear, the auto-linking/crossref for this won't work in the playground: https://github.com/eclipse-langium/langium/discussions/1502)
*/
Project infers Expr:
    InfixPredicateApplication ({infer Project.left=current} GENITIVE right=Ref)*
    // Right can't be [NamedElement:ID]
    // because we want the scoper to visit `left` first,
    // so that if `left`'s reference will be resolved via the default scoper/linker, if that's possible.
    // This allows us to then use that resolved reference
    // as a base case in figuring out the reference for `right`
;

/* ---------------------------------------------------
Aug 24 2024: Join is currently disabled;
am porting what I currently have for Join to Project
-------------------------------------------------------*/


InfixPredicateApplication infers Expr:
    InfixActionApplication
    (
        {infer InfixPredicateApplication.left = current}
        predicate=[PredicateDecl:IDOrBackTickedID]
        right=InfixActionApplication?
    )*
;

InfixActionApplication infers Expr:
    Cons
    (
        {infer InfixActionApplication.left = current}
        ("DOES" | "DO")
        // ("DOES" | "DO") is a short term hack to avoid clashing with InfixPredicateApplication.
        // TODO: Might want to just overload the surface syntax for action/predicate application and use type annotations / inference to distinguish them?
        // Also, may want to add an infix fun app and then use Langium actions to desugar this to an infix fun app?
        action=[ActionDecl:IDOrBackTickedID]
        right=Cons?
    )*
;

Cons infers Expr:
    PostfixPredicateApplication
    ({infer Cons.first=current}
     'followed_by_items_in'
     rest=PostfixPredicateApplication
    )*
;

fragment ExprArgs:
    (args+=Expr)? (',' args+=Expr)*
;

fragment ExprArgsInParens:
    '(' ExprArgs ')'
;


PostfixPredicateApplication:
    FunctionApplication
    ({infer PostfixPredicateApplication.predicate=current}

    ( ( ('IS_SATISFIED_BY' | 'HOLDS_OF') ExprArgsInParens )
    | 'HOLDS?' )

    )*
;


FunctionApplication:
    PrimitiveExpr
    ({infer FunctionApplication.func=current}
    ExprArgsInParens)*
;


// sugar for a predicate that checks if the referenced norm is violated
NormIsInfringed:
    normRef=[Statement:IDOrBackTickedID] 'IS_INFRINGED'
;

// This is meant for programmers to implement libraries with;
// it's not something I'm expecting ordinary, non-programmer users to use or understand
AnonFunction:
    '\\'
    AnonFunParams
    '=>'
    body=Expr
;

/* ============================
    List and ListOps
=============================== */

List:
    EmptyList | NonEmptyList
;

EmptyList:
    {infer List}
    'EMPTY_LIST'
;

NonEmptyList infers List:
    'LIST_OF' elements+=Expr (',' elements+=Expr)* ('.')?
;

// TODO: Think more about what kind of surface syntax would be intuitive for foldr and foldl
Foldr:
    'FOLD_RIGHT' 'using' combine=Expr 'starting_with' nil=Expr 'over' collection=Expr
;

Foldl:
    'FOLD_LEFT' 'using' update=Expr 'starting_with' initial=Expr 'over' collection=Expr
;

MaximumOf:
    'MAXIMUM_OF' collection=Expr
;

MinimumOf:
    'MINIMUM_OF' collection=Expr
;

SumOf:
    'SUM_OF' collection=Expr
;

fragment ListOps: Foldr | Foldl | MaximumOf | MinimumOf | SumOf;

PrimitiveExpr infers Expr:
    '(' Expr ')' | AnonFunction | List | ListOps | LetExpr
    | NormIsInfringed | Ref | UnaryExpr | IntegerLiteral | BooleanLiteral | StringLiteral
    // Not sure if we want to allow for string literals as expressions
;

Ref:
    value=[NamedElement:IDOrBackTickedID];

UnaryExpr:
    op=(OpMinus | OpNot) value=Expr
;

// Denotes entrypoint / what to evaluate. Basically Simala's #eval
// TODO: Not sure about the 'Report' surface syntax
ReportDecl:
    "@REPORT" value=Expr
;

/* ============================
    Statements
=============================== */

Statement:
    VarDeclStmt
    | IfStmt | Norm
    | StatementRef
;

StatementRef:
    value=[Statement:IDOrBackTickedID]
;

/* For top level var declarations (as opposed to within the context of a LET ... IN)
   TODO: Not clear that we really want `DEFINE` surface syntax for this, given that the sorts of "Definitions" one sees in legal texts are more like type declarations
   Alternatives include:
        * FACT ...
        * LET (but without IN)
*/
VarDeclStmt:
    description=SINGLELINE_METADATA_ANNOTATION?
    'DEFINE' Var
;

Action:
    PrimAction | PrimActionBlock
;

PrimAction:
    Assign | Increment | Decrement | ActionRef
;

ActionRef:
    value=[ActionDecl:IDOrBackTickedID]
;

Assign:
    left=[NamedElement:IDOrBackTickedID] ('=' | 'becomes') right=Expr
;

// Sugar for / specific cases of Assign
Increment:
    left=[NamedElement:IDOrBackTickedID] ('+=' | 'increases_by') right=Expr
;
Decrement:
    left=[NamedElement:IDOrBackTickedID] ('-=' | 'decreases_by') right=Expr
;

fragment OrigRuleRefWithOptionalName:
    originalRuleRef=OriginalRuleRef (':' name=IDOrBackTickedID)?
;

IfStmt:
    OrigRuleRefWithOptionalName?

    // TODO: add a validator that allows a name to be supplied
    // if and only if there's a deontic in either the Then or Else.

    'IF'         condition=Expr
    'THEN'       then=(Statement | StatementBlock)
    ('OTHERWISE' else=(Statement | StatementBlock))?
    // ('UNLESS' defeaters=+Expr)? // for v2
;

ActionDecl:
    GivenParamBlock?
    'ACTION' name=IDOrBackTickedID
    ('=' PrimActionBlock)?
;

fragment PrimActionBlock: 'DO'
    actions+=PrimAction ('then' actions+=PrimAction)*
'END';

fragment StatementBlock: 'DO'
    statements+=Statement+
'END';

Norm:
    originalRuleRef=OriginalRuleRef?
    (':' name=IDOrBackTickedID)?
    // TODO: Add validator to prevent users from adding a rule ref or name here
    // if the deontic appears in the scope of another Statement that already has a rule ref or name

    agent=[NamedElement:ID]
    deonticModal=DeonticModal
    action=Action
;

// Will add sugar like prohibitions later
DeonticModal: Must | May;

Must: {infer Must} 'MUST';

May: {infer May} 'MAY';

/* ============================
   A normal record
=============================== */

RecordDecl:
    WithSpecificMetadataBlock?

    'STRUCTURE'
    name=IDOrBackTickedID
    ( 'SPECIALIZES' parents+=[RecordDecl:IDOrBackTickedID] ( ',' parents+=[RecordDecl:IDOrBackTickedID] )* )?
        rowTypes+=RowType*
    'END'
;


/* ========================================
       Sigs / Concepts
======================================== */

/* -----------------------------------------
  Aug 24 2024: Join is currently disabled
--------------------------------------------

There's a good chance I'll just give up on the Alloy stuff.

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

    multiplicity=SigMultOne? 'CONCEPT'
    name=IDOrBackTickedID
    ( 'SPECIALIZES' parents+=[SigDecl:IDOrBackTickedID] ( ',' parents+=[SigDecl:IDOrBackTickedID] )* )?
        relations+=Relation*
    'END'
;

SigMultOne: {infer SigMultOne} 'ONE';


type CustomType = SigDecl | RecordDecl;
// CustomType:
//     SigDecl
// | UnionType
// | Alias
// ;

RowType:
    // WithSpecificMetadataBlock?
    description=SINGLELINE_METADATA_ANNOTATION?

    name=IDOrBackTickedID
    (':' | 'IS_A')
    value=TypeAnnot

;

Relation:
    // WithSpecificMetadataBlock?
    description=SINGLELINE_METADATA_ANNOTATION?

    name=IDOrBackTickedID
    // optional?='?'?
    (':' | 'IS_A')
    relatum=TypeAnnot // TODO: Support plural relata
;

/* ========================================
       TypeAnnot
=========================================== */

// Not expecting non-programmers to understand the more complicated type annotations in the textual form
// This would be visualized / rendered in more accessible ways for non-programmers
TypeAnnot:
    // Note the quantity operator and the self-reference at the end. (If left-associative, would be `*` instead of `?`).
    // This is because`=>` is right-associative (c.f. https://www.typefox.io/blog/parsing-expressions-with-xtext/)
    PrimitiveTypeAnnot ({infer TypeAnnot.left=current} '=>' right=TypeAnnot)?
;

PrimitiveTypeAnnot infers TypeAnnot:
    CustomTypeDef | 'LIST_OF' TypeAnnot | BuiltinType | '(' TypeAnnot ')'
;

CustomTypeDef:
    annot = [CustomType:ID]
;

/* ========================================
       NamedElement, ToplevelStatement
=========================================== */

type NamedElement = VarBinding | Param | PredicateDecl | FunDecl | Relation | SigDecl | RowType | RecordDecl | VarDeclStmt | ActionDecl | IfStmt | Norm;

fragment ToplevelStatement: VarDeclStmt | ActionDecl | IfStmt | Norm;

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
========================================== */

// Literal: IntegerLiteral | StringLiteral | BooleanLiteral;

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


/* =======================================
        TERMINALS
========================================== */
terminal GENITIVE: "'s "; 
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
