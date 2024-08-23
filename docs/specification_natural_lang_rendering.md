# Proposal (VERY WIP) for the Natural-Language-y Rendering

## The idea

[YM TO ADD]

## Why

[YM TO ADD]

## Specification

### Ideas for the rendering

#### Goals 

Our goal in the short term can just be to come up with something that we can use to get more fine-grained feedback from stakeholders --- we shouldn't feel like we have to totally figure out the phrasing ourselves. But we probably do want to at least get to the stage where it looks reasonable to a non-engineer before approaching stakeholders.

Other desiderata are as follows.

##### Software Design

* Whether to use GF feels like an implementational detail. We should try to come up with an 'interface' that is agnostic in its implementation; it should be easy to experiment with different implementations of the renderer.
* Probably want some kind of intermediate representation that is more structured than HTML --- an IR that still preserves info about whether something is, e.g., a name of a record, so that we can apply semantic formatting when doing the final 'rendering' (e.g., have names of records be formatted so that they look the way that defined terms look in contracts, and so forth). The IR should also include enough info that we can trace which regions of the source program correspond to a given part of the rendered stuff.

I'll try to look more carefully at John C's stuff again, and look into LSP-related architectures, to get ideas for the broad software design. 

##### Filtering

One huge advantage of the rendering route is that we can really try to filter out less-relevant-to-humans stuff --- e.g. a 'for-busy-executives' mode that only shows the high-level ideas and logic.

#### Other phrasing related things

We can look to Attempto (http://attempto.ifi.uzh.ch/) for ideas re phrasing, as well as textbooks on legal drafting. 

In the short term, we can focus on contracts and criminal law, since those are the areas where we currently have people we can ask for feedback.

I was going to offer some specific phrasing ideas, but then I realized it's actually non-trivial to figure out what information, and how much of it, to present in the rendering, even just for records. 

Consider, e.g., this example (https://github.com/CatalaLang/catala-examples/blob/master/NSW_community_gaming/nsw_art_union.catala_en) from Catala:

The `ArtUnion` record might be declared as follows in Lam4 (but don't try it just yet --- I haven't added proper record syntax to the Langium grammar yet):

```lam4
-- An ArtUnion is a lottery where the winners are chosen through a ticket draw.
STRUCTURE ArtUnion {
  grossProceeds: Money
  typeOfOrg: BenefitingOrganisation
  proceedsToBenefitingOrg: Money
  <... and so forth ...>  
}
```

But do we even want to explicitly render this record / structure in the default rendering mode? Would it be better instead to have that be something that one could click to, but that is 'hidden' by default, given how we don't always declare these sorts of structures upfront in the natural-language text?