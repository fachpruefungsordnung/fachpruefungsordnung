{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Ltml.ToLaTeX.GlobalState
    ( GlobalState (..)
    , CounterState (..)
    , nextSupersection
    , nextSection
    , nextParagraph
    , nextSentence
    , nextFootnote
    , nextAppendix
    , insertRefLabel
    , nextEnumPosition
    , descendEnumTree
    , addTOCEntry
    , counterState
    , supersectionCTR
    , sectionCTR
    , paragraphCTR
    , sentenceCTR
    , footnoteCTR
    , appendixCTR
    , enumPosition
    , enumIdentifier
    , appendixFormat
    , onlyOneParagraph
    , isSupersection
    , isAppendix
    , labelToRef
    , labelToFootNote
    , toc
    , initialGlobalState
    , initialCounterState
    ) where

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.DList as DList
import Data.Map (Map, insert)
import qualified Data.Text.Lazy as LT
import Language.Lsd.AST.Format (IdentifierFormat, KeyFormat)
import Language.Lsd.AST.Type.AppendixSection (AppendixElementFormat)
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.ToLaTeX.Format
    ( emptyAppendixFormat
    , emptyIdentifierFormat
    , formatKey
    , getIdentifier
    )
import Language.Ltml.ToLaTeX.Type (LaTeX (Text), linebreak)

-- State for labeling
data GlobalState = GlobalState
    { {- Counters to keep track of the position in the document -}
      _counterState :: CounterState
    , {- Path for current enum position -}
      _enumPosition :: [Int]
    , {- since the style of the identifier is defined globally for an
         enumeration or appendix we need to pass it to the kids -}
      _enumIdentifier :: IdentifierFormat
    , _appendixFormat :: AppendixElementFormat
    , {- Flags for special cases -}
      _onlyOneParagraph :: Bool -- needed for sections with only one paragraphs
    , _isSupersection :: Bool -- needed for heading
    , _isAppendix :: Bool -- needed for appendix sections
    {- Maps for labels -}
    , _labelToRef :: Map Label LT.Text
    , _labelToFootNote :: Map Label Footnote
    , {- functional list that builds the table of contents -}
      _toc :: DList.DList LaTeX
    }
    deriving (Show)

data CounterState = CounterState
    { _supersectionCTR :: Int
    , _sectionCTR :: Int
    , _paragraphCTR :: Int
    , _sentenceCTR :: Int
    , _footnoteCTR :: Int
    , _appendixCTR :: Int
    }
    deriving (Show)

makeLenses ''GlobalState
makeLenses ''CounterState

nextSupersection :: State GlobalState Int
nextSupersection = do
    counterState . supersectionCTR <+= 1

nextSection :: State GlobalState Int
nextSection = do
    counterState . sectionCTR <+= 1

nextParagraph :: State GlobalState Int
nextParagraph = do
    counterState . sentenceCTR .= 0
    counterState . paragraphCTR <+= 1

nextSentence :: State GlobalState Int
nextSentence = do
    counterState . sentenceCTR <+= 1

nextFootnote :: State GlobalState Int
nextFootnote = do
    counterState . footnoteCTR <+= 1

nextAppendix :: State GlobalState Int
nextAppendix = do
    counterState . appendixCTR <+= 1

-- Get the next label at the current depth
nextEnumPosition :: State GlobalState [Int]
nextEnumPosition = do
    prefix <- use enumPosition
    let depth = length prefix
        newPrefix = init prefix ++ [prefix !! (depth - 1) + 1]
    enumPosition .= newPrefix
    pure newPrefix

-- Go one level deeper temporarily
descendEnumTree :: State GlobalState a -> State GlobalState a
descendEnumTree action = do
    oldPath <- use enumPosition
    enumPosition .= oldPath ++ [0]
    result <- action
    enumPosition .= oldPath
    pure result

insertRefLabel :: Maybe Label -> LT.Text -> State GlobalState ()
insertRefLabel mLabel ident =
    forM_ mLabel $ \l -> labelToRef %= insert l ident

addTOCEntry
    :: Int -> KeyFormat -> IdentifierFormat -> LaTeX -> State GlobalState ()
addTOCEntry n keyident ident headingText =
    toc
        %= ( <>
                DList.fromList
                    [ formatKey keyident (Text $ getIdentifier ident n)
                    , Text " "
                    , headingText
                    , linebreak
                    ]
           )

initialGlobalState :: GlobalState
initialGlobalState =
    GlobalState
        initialCounterState
        [0]
        emptyIdentifierFormat
        emptyAppendixFormat
        False
        False
        False
        mempty
        mempty
        mempty

initialCounterState :: CounterState
initialCounterState =
    CounterState
        0
        0
        0
        0
        0
        0
