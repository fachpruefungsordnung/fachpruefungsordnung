{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.ToLaTeX ( intermedateToLaTeX )
where
import Language.Ltml.ToLaTeX.LaTeXType (LaTeX (..))
import Language.Ltml.ToLaTeX.PreLaTeXType (PreLaTeX (..))
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import Language.Ltml.AST.Label (Label(Label))

intermedateToLaTeX :: Map.Map Label LT.Text -> PreLaTeX -> LaTeX
intermedateToLaTeX _ (IText t) = Text t
intermedateToLaTeX _ (IRaw r) = Raw r
intermedateToLaTeX _ (ICommandS n) = CommandS n 
intermedateToLaTeX m (ICommand n opts args) = Command n opts (map (intermedateToLaTeX m) args)
intermedateToLaTeX m (IEnvironment n opts content) = Environment n opts (map (intermedateToLaTeX m) content)
intermedateToLaTeX m (IBraced content) = Braced $ intermedateToLaTeX m content
intermedateToLaTeX m (ISequence content) = Sequence $ map (intermedateToLaTeX m) content
intermedateToLaTeX m (MissingRef l@(Label t)) = 
    case Map.lookup l m of
        Nothing -> Braced $ Command "Large" [] [Text "??"]
        Just ref -> Command "hyperlink" [] [Text (LT.fromStrict t), Text ref]




