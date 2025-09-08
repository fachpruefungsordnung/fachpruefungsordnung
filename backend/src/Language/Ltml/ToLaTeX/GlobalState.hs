{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.Ltml.ToLaTeX.GlobalState
    ( GlobalState (..)
    , DocType (..)
    {- functions to mutate counters -}
    , nextSupersection
    , nextSection
    , nextParagraph
    , nextSentence
    , nextFootnote
    , nextAppendix
    , resetCountersHard
    , resetCountersSoft
    {- other helper functions to mutate the globalstate -}
    , insertRefLabel
    , nextEnumPosition
    , descendEnumTree
    , addTOCEntry
    , addAppendixHeaderEntry
    , addHeaderFooter
    {- lenses -}
    , counterState
    , flagState
    , formatState
    , supersectionCTR
    , sectionCTR
    , paragraphCTR
    , sentenceCTR
    , footnoteCTR
    , appendixCTR
    , enumPosition
    , enumIdentifierFormat
    , appendixFormat
    , docHeadingFormat
    , onlyOneParagraph
    , isSupersection
    , flaggedParent
    , flaggedChildren
    , docType
    , labelToRef
    , labelToFootNote
    , toc
    , appendixHeaders
    , preDocument
    {- initial states -}
    , initialGlobalState
    , initialCounterState
    , initialFlagState
    ) where

import Control.Lens (makeLenses, use, (%=), (.=), (<+=))
import Control.Monad (forM_)
import Control.Monad.State (State)

import qualified Data.DList as DList
import Data.Map (Map, insert)
import qualified Data.Text as T
import Language.Lsd.AST.Format (IdentifierFormat, KeyFormat, MainHeadingFormat)
import Language.Lsd.AST.Type.AppendixSection (AppendixElementFormat)
import Language.Lsd.AST.Type.DocumentContainer
    ( HeaderFooterFormat (HeaderFooterFormat)
    )
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.ToLaTeX.Format
    ( emptyAppendixFormat
    , emptyHeadingFormat
    , emptyIdentifierFormat
    , formatHeaderFooterItem
    , formatKey
    , getIdentifier
    , staticDocumentFormat
    )
import Language.Ltml.ToLaTeX.PreLaTeXType
    ( PreLaTeX (ISequence, IText)
    , fancyfoot
    , fancyhead
    , linebreak
    )

-- State for labeling
data GlobalState = GlobalState
    { {- Counters to keep track of the position in the document -}
      _counterState :: CounterState
    , {- Flags for special cases -}
      _flagState :: FlagState
    , _formatState :: FormatState
    , {- Path for current enum position -}
      _enumPosition :: [Int]
    , {- since the style of the identifier is defined globally for an
         enumeration or appendix we need to pass it to the kids -}
      {- Maps for labels -}
      _labelToRef :: Map Label T.Text
    , _labelToFootNote :: Map Label Footnote
    , {- functional list that builds the table of contents -}
      _toc :: DList.DList PreLaTeX
    , _appendixHeaders :: DList.DList PreLaTeX
    , {- pre-document is used to store the header and footer of the document -}
      _preDocument :: PreLaTeX
    }
    deriving (Show)

data CounterState = CounterState
    { _supersectionCTR :: Int
    , _sectionCTR :: Int
    , _paragraphCTR :: Int
    , _sentenceCTR :: Int
    , _footnoteCTR :: Int
    , _appendixCTR :: Int
    , _documentCTR :: Int
    }
    deriving (Show)

data FlagState = FlagState
    { _onlyOneParagraph :: Bool -- needed for sections with only one paragraph
    , _isSupersection :: Bool -- needed for heading
    , _flaggedParent :: Bool
    , _flaggedChildren :: Bool
    , _docType :: DocType -- needed to distinguish between main document and appendix
    }
    deriving (Show)

data DocType = Main | Appendix
    deriving (Show, Eq)

data FormatState = FormatState
    { _docHeadingFormat :: MainHeadingFormat
    , _appendixFormat :: AppendixElementFormat
    , _enumIdentifierFormat :: IdentifierFormat
    }
    deriving (Show)

makeLenses ''GlobalState
makeLenses ''CounterState
makeLenses ''FlagState
makeLenses ''FormatState

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

resetCountersHard :: State GlobalState ()
resetCountersHard = do
    counterState . supersectionCTR .= 0
    counterState . sectionCTR .= 0
    counterState . paragraphCTR .= 0
    counterState . footnoteCTR .= 0
    counterState . appendixCTR .= 0

resetCountersSoft :: State GlobalState ()
resetCountersSoft = do
    counterState . supersectionCTR .= 0
    counterState . sectionCTR .= 0
    counterState . paragraphCTR .= 0
    counterState . footnoteCTR .= 0

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

insertRefLabel :: Maybe Label -> T.Text -> State GlobalState ()
insertRefLabel mLabel ident =
    forM_ mLabel $ \l -> labelToRef %= insert l ident

addTOCEntry
    :: Int -> KeyFormat -> IdentifierFormat -> PreLaTeX -> State GlobalState ()
addTOCEntry n keyident ident headingText =
    toc
        %= ( <>
                DList.fromList
                    [ formatKey keyident (IText $ getIdentifier ident n)
                    , IText " "
                    , headingText
                    , linebreak
                    ]
           )

addAppendixHeaderEntry
    :: Int -> KeyFormat -> IdentifierFormat -> PreLaTeX -> State GlobalState ()
addAppendixHeaderEntry n keyident ident headingText =
    appendixHeaders
        %= ( <>
                DList.fromList
                    [ formatKey keyident (IText $ getIdentifier ident n)
                    , IText " "
                    , headingText
                    , linebreak
                    ]
           )

addHeaderFooter
    :: HeaderFooterFormat
    -> HeaderFooterFormat
    -> T.Text
    -> T.Text
    -> T.Text
    -> State GlobalState ()
addHeaderFooter
    (HeaderFooterFormat topLeft topCenter topRight)
    (HeaderFooterFormat botLeft botCenter botRight)
    superTitle
    title
    date = do
        let superTitle' = IText superTitle
            title' = IText title
            date' = IText date
            assemble items = ISequence $ map (formatHeaderFooterItem superTitle' title' date') items
        preDocument
            %= ( <>
                    ISequence
                        [ fancyhead ["l"] (assemble topLeft)
                        , fancyhead ["c"] (assemble topCenter)
                        , fancyhead ["r"] (assemble topRight)
                        , fancyfoot ["l"] (assemble botLeft)
                        , fancyfoot ["c"] (assemble botCenter)
                        , fancyfoot ["r"] (assemble botRight)
                        ]
               )
initialGlobalState :: GlobalState
initialGlobalState =
    GlobalState
        initialCounterState
        initialFlagState
        initialFormatState
        [0]
        mempty
        mempty
        mempty
        mempty
        staticDocumentFormat

initialCounterState :: CounterState
initialCounterState =
    CounterState
        0
        0
        0
        0
        0
        0
        0

initialFlagState :: FlagState
initialFlagState =
    FlagState
        False -- onlyOneParagraph
        False -- isSupersection
        False
        False
        Main -- isAppendix

initialFormatState :: FormatState
initialFormatState =
    FormatState
        emptyHeadingFormat
        emptyAppendixFormat
        emptyIdentifierFormat
