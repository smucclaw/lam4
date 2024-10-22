# Taking stock of Lam4

## Syntax / grammar

### Data declarations

Currently quite limited: only product types like RecordDecls (and a specific kind of SigDecl), though can simulate closed sums as follows:

```
data Animal
= Dog String Bool
| Cat String Integer
```

can be simulated (ish -- won't get the closedness) with

```lam4
STRUCTURE Animal END

STRUCTURE Dog SPECIALIZES Animal
    name: String
    isFriendly: Boolean
END

STRUCTURE Cat SPECIALIZES Animal
    name: String
    age: Integer
END
```

Would be good to add closed sum types and enums.

### Annotations

#### Annotations for 'nautral language generation / rendering templates / knobs'

Has not been implemented, but as Inari pointed out, this would be helpful, and some of the current annotation syntax had been designed to be readily extended with things like that.

```langium
RecordDecl:
    WithSpecificMetadataBlock?

    'STRUCTURE'
    name=IDOrBackTickedID
    ( 'SPECIALIZES' parents+=[RecordDecl:IDOrBackTickedID] ( ',' parents+=[RecordDecl:IDOrBackTickedID] )* )?
        rowTypes+=RowType*
    'END'
;

fragment WithSpecificMetadataBlock:
    metadata=SpecificMetadataBlock
;

SpecificMetadataBlock:
    '/-'
        // 'About' instead of 'about' b/c that feels more in line with legal style
        // TODO: not sure if we want the delimiters to be double quotes
        ("About:" description=StringLiteral)?

        // TODO: Add constructs for annotating / adjusting what the rendered explanations look like
    '-/'
;
```

Example:

```lam4
/-
  About: "Information about the life assured for this policy."
-/
STRUCTURE LifeAssured
  -- whether policy active  
  policy_active: Boolean
  -- how old the life assured is
  age: Integer
END
```

And of course, we could and should add constructs for annotating levels of transparency / explanation depth too.

#### Sidenote on the SpecificMetadataBlock syntax and how it might be displayed in editors like CodeMirror that allow for inline GUI widgets

The idea had been that in something like a CodeMirror-based editor that's targeted at a non-technical user, we could render the SpecificMetadataBlock as a panel with form fields or GUI knobs / sliders (e.g., sliders would be a natural fit for things like levels of 'transparency / explanation depth'). That is, the intent behind the SpecificMetadataBlock surface syntax was that there should be a natural correspondence between the 'ASCII' version of it and the more GUI-ControlPanel rendering of it.

#### Annotations for original source citations

```langium
OriginalRuleRef:
    ('SECTION' | '§') section=INTEGER
;
```

##### Where this is currently supported

The following constructs allow for annotation with the original source citation:

* FunDecl, Norm, PredicateDecl

##### What could be better

###### The section reference parser

should be augmented to allow for references like `§10(a)` or `§10(1)(a)`, and not just `§10`

###### Flexibility in annotation order

Currently the syntax enforces a certain order re the annotations, e.g.

```langium
PredicateDecl:
    IsEntrypoint
    description=SINGLELINE_METADATA_ANNOTATION?
    originalRuleRef=OriginalRuleRef?
    ...
```

This was done for purely cosmetic reasons.
But it may be better to just have an extensible block of annotations that can be in any order, in the same way that the RecordDecl currently does it.

#### Annotations for entrypoint

terminal: `@FOR_Q&A`

Supported by

* FunDecl, PredicateDecl, VarDeclStmt

But the same issues with the annotation order being fixed applies here too.

#### Annotations for evaluation

```langium
// Denotes what to evaluate. Basically Simala's #eval
// TODO: Not sure about the 'Report' surface syntax
ReportDecl:
    "@REPORT" value=Expr
;
```

#### Annotations for description

This is not a comment.

`terminal SINGLELINE_METADATA_ANNOTATION: /--([^\n\r]*)/;`

Supported by

* FunDecl, PredicateDecl, record field member, relation (i.e., sig decl member)


### Normative constructs

See https://github.com/smucclaw/lam4/blob/main/docs/proposal_normative_constructs_aug26_2024.md

What I had basically been trying to do with this was to come up with a first draft for a syntax that:

1. is based on what I took to be a common core in the grammar / syntaxes I had seen in various normative DSLs

2. is more compositional and human-understandable than the Spreadsheet L4 syntax for normative constructs

Re 1: the syntax in the proposal does go beyond, e.g., the Contract Automata paper's CL_rest grammar in that it also adds, e.g., assignments of variables. But I tried to limit myself to additions that seemed relatively straightforward / non-controversial.

The syntax I proposed doesn't currently have 'first-class' support for timing constraints --- I took those to be more like a v2 / v3 feature.


### Other constructs that need to be added

The langium grammar has

```langium
MaximumOf:
    'MAXIMUM_OF' collection=Expr
;

MinimumOf:
    'MINIMUM_OF' collection=Expr
;

SumOf:
    'SUM_OF' collection=Expr
;
```

but these haven't been implemented in the backend parser (Parser.hs). It'll only take like 5-10 min to do that; just haven't got around to doing it.



### Rough edges that need to be smoothed out

#### The ReportDecl (@REPORT) annotation is currently implemented in a different way from the other annotations; may want to standardize the implementation

#### Somewhat inconsistent use of braces vs `...END`

Ideally, most of the things that currently use  either braces or `...END` as delimiters would be refactored to use indentation instead.

E.g.:

```
LET {
    z = 2,
    x = 1
} IN {
    LET { x = 100} IN { x }
}
```

should just become

```
LET
  z = 2,
  x = 1
IN
  LET x = 100 IN x
```

And similarly,

```
FUNCTION Integer => Integer
g(x) = x * 2 - 1 / 3
END
```

could become

```
FUNCTION Integer => Integer
  g(x) = x * 2 - 1 / 3
```

Now, there are some uses of braces that don't quite fall into this category, e.g. the use of braces in InfixPredApp:

```lam4
GIVEN (some_person: Person
       another_person: Person)
DECIDE {some_person} `has helped` {another_person}
IF True
```

Those can probably be refactored to avoid any sort of delimiter with more complicated parsing techniques.


#### `DEFINE`

`DEFINE` is basically variable declaration and definition, e.g.

```lam4
DEFINE ceilinged  = ceiling 0.5
```

I had gone with this because it's what the legacy Spreadsheet L4 syntax uses. But I suspect `DEFINE` might be misleading or confusing to a non-programmer in a legal / regulations context, since legal definitions (i.e., things like "Definition ...: ...." are often data declarations.

#### Allcaps keywords

* Not obvious that the all-caps keywords are helpful if we have an environment that allows us to do syntax highlighting / bolding / italicizing

* I personally find  the all-caps keywords often makes the code harder to read


## Whether to use Langium
