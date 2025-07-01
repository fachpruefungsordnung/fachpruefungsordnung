{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToPandoc.Text
    ( textBlockW
    , inlineTextW
    )
where

import Data.Either.Utils (leftMerge, leftMergeMap)
import Data.List (singleton)
import Data.Text (Text)
import Data.Void (Void, absurd)
import Language.Ltml.AST.Label (unLabel)
import Language.Ltml.AST.Text
    ( EnumItem (EnumItem)
    , Enumeration (Enumeration)
    , FontStyle (..)
    , SentenceStart (SentenceStart)
    , TextTree (..)
    )
import Language.Ltml.ToPandoc (ToPandoc, getVisibleIdent)
import qualified Text.Pandoc.Definition as P
    ( Block (OrderedList, Plain)
    , Inline (..)
    , ListNumberDelim (DefaultDelim)
    , ListNumberStyle (DefaultStyle)
    , Target
    )

textBlockW
    :: (StyleW style, EnumW enum block, SpecialW special)
    => [TextTree style enum special]
    -> ToPandoc [P.Block]
textBlockW xs = leftMergeMap P.Plain toBlock . concat <$> mapM textTreeW xs

inlineTextW
    :: (StyleW style, SpecialW special)
    => [TextTree style Void special]
    -> ToPandoc [P.Inline]
inlineTextW xs = fmap (either id absurd) . concat <$> mapM textTreeW xs

textTreeW
    :: (StyleW style, EnumW enum block, SpecialW special)
    => TextTree style enum special
    -> ToPandoc [Either P.Inline block]
textTreeW (Word w) = rsi $ P.Str w
textTreeW Space = rsi P.Space
textTreeW (Special special) = rsi $ specialW special
textTreeW (Reference lbl) = do
    (ident, tgt) <- f <$> getVisibleIdent lbl
    rsi $ P.Link ("", [], []) [P.Str ident] tgt
  where
    f :: Maybe Text -> (Text, P.Target)
    f Nothing = ("??", ("", "Broken reference"))
    f (Just ident) = (ident, ("#" <> unLabel lbl, "" {- TODO -}))
textTreeW (Styled style xs) =
    leftMerge (styled style) . concat <$> mapM textTreeW xs
textTreeW (Enum enum) = sb <$> enumW enum
textTreeW (Footnote xs) = si . P.Note <$> textBlockW xs

rsi :: P.Inline -> ToPandoc [Either P.Inline block]
rsi = return . si

si :: P.Inline -> [Either P.Inline block]
si = singleton . Left

sb :: block -> [Either P.Inline block]
sb = singleton . Right

class SpecialW special where
    specialW :: special -> P.Inline

instance SpecialW Void where
    specialW = absurd

instance SpecialW SentenceStart where
    specialW (SentenceStart Nothing) = P.Span ("", [], []) []
    specialW (SentenceStart (Just lbl)) = P.Span (unLabel lbl, [], []) []

class StyleW style where
    styled :: style -> [P.Inline] -> P.Inline

instance StyleW Void where
    styled = absurd

instance StyleW FontStyle where
    styled Bold = P.Strong
    styled Italics = P.Emph
    styled Underlined = P.Underline

class (ToBlock block) => EnumW enum block | enum -> block where
    enumW :: enum -> ToPandoc block

instance EnumW Void Void where
    enumW = absurd

instance EnumW Enumeration P.Block where
    enumW (Enumeration items) =
        P.OrderedList (1, P.DefaultStyle, P.DefaultDelim)
            <$> mapM enumItemW items
      where
        enumItemW (EnumItem xs) = textBlockW xs

class ToBlock block where
    toBlock :: block -> P.Block

instance ToBlock Void where
    toBlock = absurd

instance ToBlock P.Block where
    toBlock = id
