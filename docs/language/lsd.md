# LTML Schema Definition (LSD)

## Overview

* An LTML Schema Definition defines *types*.
* Each type is of a specific *kind*.
* There is a pre-defined set of [type constructors](lsd/type-constructors.md).
    * Each type constructor yields types of a specific kind.
* An LSD should normally define at least one `document` type (type of kind
  `document`).


## Syntax

* The syntax is defined by the [EBNF grammar](lsd/syntax.ebnf.txt) together
  with the set of [type constructors](lsd/type-constructors.md).
* Parsing LSD is not yet implemented (TODO); relatedly, the syntax is likely
  to change.
