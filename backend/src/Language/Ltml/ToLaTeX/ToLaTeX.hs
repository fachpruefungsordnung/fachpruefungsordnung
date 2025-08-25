{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.ToLaTeX (toLaTeX)
where

import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import Language.Ltml.AST.Label (Label (Label))
import Language.Ltml.ToLaTeX.LaTeXType (LaTeX (..))
import Language.Ltml.ToLaTeX.PreLaTeXType (PreLaTeX (..))

toLaTeX :: Map.Map Label LT.Text -> PreLaTeX -> LaTeX
toLaTeX _ (IText t) = Text t
toLaTeX _ (IRaw r) = Raw r
toLaTeX _ (ICommandS n) = CommandS n
toLaTeX m (ICommand n opts args) = Command n opts (map (toLaTeX m) args)
toLaTeX m (IEnvironment n opts content) = Environment n opts (map (toLaTeX m) content)
toLaTeX m (IBraced content) = Braced $ toLaTeX m content
toLaTeX m (ISequence content) = Sequence $ map (toLaTeX m) content
toLaTeX m (MissingRef l@(Label t)) =
    case Map.lookup l m of
        Nothing -> Braced $ Command "Large" [] [Text "??"]
        Just ref ->
            Command
                "hyperlink"
                []
                [ Text (LT.fromStrict t)
                , Text ref
                ]
