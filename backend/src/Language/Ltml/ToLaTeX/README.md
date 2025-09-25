# ToLaTeX

This folder provides all means to turn the AST into latex code
and render it to a pdf bytestream

# Module structure

To this end different parts of the pipeline are module seperated
to keep a clean structure:

- **PDFGenerator.hs** : this is the top level module that contains the function
    to generate the final pdf bytestream from an AST.
- **ToPreLaTeXM.hs** : this is the core of the translation process, as it provides the
    means to translate the AST into the PreLaTeX and therefore also
    the LaTeX structure. it provides the class ToPreLaTeXM and instances
    for all major parts of the AST.
- **Format.hs** : this module provides the means to handle a lot of 
    `*Format` data types from `Language/Lsd/AST`
- **GlobalState.hs** : this module contains the definition of the state
    that is used in *ToPreLaTeXM.hs*, as well as a lot of 
    helper functions
- **LaTeXType.hs** : this module contains the definition of the `LaTeX` datatype
    that is used to render the final latex code
- **PreLaTeXType.hs** : this module provides an intermediate data type that 
    can be extended upon. the seperation into a `LaTeX` and a `PreLaTeX` type is
    used to keep the final latex structure clean of stuff that is not actually
    latex syntax.
- **Renderer.hs** : this module provides a function to generate latex code
    as text from a given latex structure. it also provides a pretty printing function
- **Testing.hs** : this module can be used to write arbitrary testing code.
    currently it provides functions to render the example fpo tree, defined in
    `Language.Ltml.Tree.Example.Fpo` into a pdf bytestream or latex code, which
    can be found in `./Auxiliary` after execution.