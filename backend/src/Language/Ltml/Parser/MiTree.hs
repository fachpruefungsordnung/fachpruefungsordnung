{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing Mixed Indentation Trees---trees that may both be represented by
--   indentation, or by bracketing tokens, where the latter nodes may span
--   multiple lines of matching indentation.
module Language.Ltml.Parser.MiTree
    ( MiElementConfig (..)
    , miForest
    , hangingBlock
    , hangingBlock'
    , hangingBlock_
    )
where

import Control.Alternative.Utils (whenAlt)
import Control.Applicative (optional, (<|>))
import Control.Applicative.Utils ((<:>))
import Control.Monad (void, when)
import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Language.Ltml.Parser (MonadParser)
import Language.Ltml.Parser.Common.Indent
    ( checkIndent
    , nextIndentLevel
    , nli
    )
import Language.Ltml.Parser.Common.Lexeme (sp, sp1)
import Text.Megaparsec (Pos)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

-- | Configuration on how to handle an element (node in a mi-tree).
data MiElementConfig = MiElementConfig
    { miecPermitEnd :: Bool
    -- ^ Whether to permit the parent element to end with this element
    --   (or else require a succeeding element).
    , miecPermitChild :: Bool
    -- ^ Whether to permit a child to succeed this element.
    , miecRetainPrecedingWhitespace :: Bool
    -- ^ whether to retain (or else drop) whitespace between the preceding and
    --   this element (if any).
    , miecRetainTrailingWhitespace :: Bool
    -- ^ Whether to retain (or else drop) whitespace between this and
    --   the subsequent element (if any).
    --   This does not apply if the subsequent element is a child (in which
    --   case whitespace is always dropped).
    }

-- | Parse a list of mixed indentation trees (a forest), terminated by a
--   newline (plus indentation).
--
--   In-line nodes are parsed by @'elementPF' p@, where @p@ is supplied as the
--   parser for the children nodes.
--
--   Indented nodes are parsed by 'childP'.
--
--   This expects that the next character is not an (ASCII) space.
--
--   'elementPF' is expected to be a simple bracketing wrapper around its
--   argument @p@, and not to consume (ASCII) spaces or newlines, unlike @p@.
--   For leaf nodes, 'elementPF' may simply ignore @p@.
--   @'elementPF' p@ is further generally expected to not accept the empty
--   input.  Exceptions are permitted when
--   (a) sufficiently repeated application eventually halts
--       (i.e. @'Text.Megaparsec.many' ('elementPF' p)@ halts), and
--   (b) @'elementPF' p@ does not succeed on an empty input line (possibly
--       with indentation / (ASCII) spaces).
--
--   Unlike 'elementPF', 'childP' is expected to take care of indentation
--   itself--except at the very beginning, where it may expect that any
--   indentation has been parsed and the indentation is correct.
--   Further, 'childP' must only succeed after a final newline (plus
--   indentation).
--   Typically, 'childP' uses 'hangingBlock', which satisfies these
--   requirements.
miForest
    :: forall m a
     . (MonadParser m, FromWhitespace [a])
    => (m [a] -> m (MiElementConfig, [a]))
    -> m a
    -> m [a]
miForest elementPF childP = L.indentLevel >>= miForestFrom elementPF childP

miForestFrom
    :: forall m a
     . (MonadParser m, FromWhitespace [a])
    => (m [a] -> m (MiElementConfig, [a]))
    -> m a
    -> Pos
    -> m [a]
miForestFrom elementPF childP lvl = go False mempty
  where
    go :: Bool -> Text -> m [a]
    go isBracketed initialPrecWS = do
        -- Permit and drop initial whitespace within brackets.
        if isBracketed
            then sp >> optional (nli >> checkIndent lvl) >> goE mempty
            else goE initialPrecWS
      where
        -- `goX` vs. `goX'`:
        --  - The `goX` parsers must generally be used in-line; that is, not
        --    at the start of a line.
        --    - Exception: `goE`, initially.
        --  - The `goX'` parsers must only be used at the start of a line
        --    (after indentation; i.e., after `nli`).

        -- Parse forest, headed by element.
        goE :: Text -> m [a]
        goE precWS = do
            (cfg, e) <- elementPF (go True mempty)

            let precWS' :: [a]
                precWS' =
                    if miecRetainPrecedingWhitespace cfg
                        then fromWhitespace precWS
                        else []

            s0 <- sp

            let f :: Text -> Text
                f =
                    if miecRetainTrailingWhitespace cfg
                        then (s0 <>)
                        else id

                wC :: m [a] -> m [a]
                wC = whenAlt $ miecPermitChild cfg

                wEnd :: m [a] -> m [a]
                wEnd = whenAlt $ miecPermitEnd cfg

                goAny :: m [a]
                goAny =
                    goE (f mempty)
                        <|> whenAlt isBracketed (wEnd goEnd)

                goAny' :: Text -> m [a]
                goAny' s =
                    goE' (f s)
                        <|> wC goC'
                        <|> wEnd goEnd'

            es <- (nli >>= goAny') <|> goAny

            return $ precWS' ++ e ++ es

        goE' :: Text -> m [a]
        goE' s = checkIndent lvl *> goE s

        -- Parse forest, headed by child; at start of an input line.
        goC' :: m [a]
        goC' = checkIndent lvl' *> childP <:> (goE' mempty <|> goC' <|> goEnd')
          where
            lvl' = nextIndentLevel lvl

        goEnd :: m [a]
        goEnd = pure []

        goEnd' :: m [a]
        goEnd' = do
            -- Check indentation of closing bracket.
            when isBracketed (checkIndent lvl)
            goEnd

-- | Parse a mi-forest headed by a keyword, with all lines but the first
--   indented one additional level.
--
--   Text may begin on the line of the keyword, with at least one separating
--   (ASCII) space, or on the next (indented) line.
--
--   The result of 'keywordP' is a function that is applied to the parsed
--   mi-forest.
--   'keywordP' is expected not to consume any whitespace.
--
--   For the other arguments, see 'miForest'.
hangingBlock
    :: (MonadParser m, FromWhitespace [a])
    => m ([a] -> b)
    -> (m [a] -> m (MiElementConfig, [a]))
    -> m a
    -> m b
hangingBlock keywordP elementPF childP = do
    lvl' <- nextIndentLevel <$> L.indentLevel
    f <- keywordP
    void sp1 <|> void nli <* checkIndent lvl'
    f <$> miForestFrom elementPF childP lvl'

-- | Version of 'hangingBlock' where the keyword parser may yield any value,
--   which is paired with the parsed mi-forest.
hangingBlock'
    :: (MonadParser m, FromWhitespace [a])
    => m b
    -> (m [a] -> m (MiElementConfig, [a]))
    -> m a
    -> m (b, [a])
hangingBlock' = hangingBlock . fmap (,)

-- | Version of 'hangingBlock' where the keyword parser does not return a
--   value.
hangingBlock_
    :: (MonadParser m, FromWhitespace [a])
    => m ()
    -> (m [a] -> m (MiElementConfig, [a]))
    -> m a
    -> m [a]
hangingBlock_ = hangingBlock . fmap (const id)
