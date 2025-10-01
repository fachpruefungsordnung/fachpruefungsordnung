# Language

We define the [Legal Text Markup language (LTML)](language/ltml.md), which is
actually a family of languages, where each language is defined by an
[LTML Schema Definition (LSD)](language/lsd.md), specified in the eponymous
language (compare XML and XSD).


## Formal grammar specification

To specify formal grammars, we generally use a
[variant of EBNF](language/ebnf.md).


## Haskell code structure

* `Language/`
    * `Lsd/`
        * `AST.Type[.*]`: LTML types and node formats.
        * `Example[.Fpo]`: Example LSD; as of now the only available LSD,
          hardcoded as such.  To be demoted to proper example once LSD can be
          parsed.
    * `Ltml/`
        * `AST.*`: LTML AST
        * `Parser[.*]`: LTML Parser
        * `Tree`: simple textual tree where the structure mirrors the
          navigation TOC in the frontend and the leafs are editable in the
          text editor in the frontend.  This simple tree structure is strongly
          related to the tree structure of a document in the database.
        * `Tree/`
            * `ToMeta`: Building of metadata for use in the frontend.
            * `ToLtml`: Main translation function.
            * `Parser[.*]`: "Parsing" simple trees to LTML.
            * `Example.Fpo`: Simple example tree.  Used as default for new
              documents.
        * `HTML[.*]`: Conversion to HTML.
        * `ToLaTex.*`: Conversion to LaTeX; to be further translated to PDF.
