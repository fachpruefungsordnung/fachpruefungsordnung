{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Language.Ltml.ToLaTeX.GlobalState (
  GlobalState (..),
  nextSupersection,
  nextSection,
  nextParagraph,
  nextSentence,
  insertLabel,
  nextEnumPosition,
  descendEnumTree
) where

import Control.Monad.State
import Language.Ltml.ToLaTeX.Type (LaTeX)
import Data.Map (Map, insert)
import Language.Ltml.AST.Label (Label)
import qualified Data.Text.Lazy as LT

-- example structure
data Supersection = Supersection [Int] [Section]
  deriving Show

data Section = Section [Int] [Paragraph]
  deriving Show

newtype Paragraph = Paragraph [Int]
  deriving Show

-- State for labeling
data GlobalState = GlobalState
  { supersection :: Int             -- counter for supersections
  , section :: Int                  -- counter for sections
  , paragraph :: Int                -- counter for paragraphs within a section
  , sentence :: Int                 -- counter for sentences within a paragraph
  , enumPosition :: [Int]           -- tracks the current position in an enum tree
  , onlyOneParagraph :: Bool        -- needed for sections with only one paragraphs
  , isSupersection :: Bool          -- needed for heading
  , identifier :: LaTeX             -- identifier for formatting
  , labelToRef :: Map Label LT.Text -- map for labels
  } deriving Show

nextSupersection :: State GlobalState Int
nextSupersection = do
  st <- get
  let n = supersection st + 1
  put st { supersection = n }
  pure n

nextSection :: State GlobalState Int
nextSection = do
  st <- get
  let n = section st + 1
  put st { section = n, paragraph = 0 }
  pure n

nextParagraph :: State GlobalState Int
nextParagraph = do
  st <- get
  let n = paragraph st + 1
  put st { paragraph = n, sentence = 0 }
  pure n

nextSentence :: State GlobalState Int
nextSentence = do
  st <- get
  let n = sentence st + 1
  put st { sentence = n }
  pure n

-- Get the next label at the current depth
nextEnumPosition :: State GlobalState [Int]
nextEnumPosition = do
  st <- get
  let prefix = enumPosition st
      depth = length prefix
      newPrefix = init prefix ++ [prefix !! (depth - 1) + 1]
  put $ st { enumPosition = newPrefix }
  pure newPrefix

-- Go one level deeper temporarily
descendEnumTree :: State GlobalState a -> State GlobalState a
descendEnumTree action = do
  st <- get
  let oldPath = enumPosition st
  put $ st { enumPosition = enumPosition st ++ [0]}
  result <- action
  modify $ \s -> s { enumPosition = oldPath }
  pure result

insertLabel :: Maybe Label -> LT.Text -> State GlobalState ()
insertLabel mLabel ident = do
  maybe (pure ()) (\l -> modify (\s -> s { labelToRef = insert l ident (labelToRef s) })) mLabel












------------------------------- example for texting -------------------------------

exampleLabelSuperSection :: Supersection -> State GlobalState Supersection
exampleLabelSuperSection (Supersection _ children) = do
  _ <- nextSupersection
  st <- get
  labeledChildren <- mapM exampleLabelSection children
  pure $ Supersection [supersection st] labeledChildren

exampleLabelSection :: Section -> State GlobalState Section
exampleLabelSection (Section _ children) = do
  _ <- nextSection
  st <- get
  labeledChildren <- mapM exampleLabelParagraph children
  pure $ Section [supersection st, section st] labeledChildren

exampleLabelParagraph :: Paragraph -> State GlobalState Paragraph
exampleLabelParagraph (Paragraph _) = do
  _ <- nextParagraph
  st <- get
  pure $ Paragraph [supersection st, section st, paragraph st] 

exampleTree :: Supersection
exampleTree = Supersection [] [
                                Section [] [ Paragraph []
                                           , Paragraph []
                                           ]
                              , Section [] [ Paragraph []
                                           , Paragraph []
                                           , Paragraph []
                                           , Paragraph []
                                           ]
                              , Section [] [ Paragraph []
                                           , Paragraph []
                                           , Paragraph []
                                           ]
                              , Section [] []
                              ]


data Tree = Empty | Node [Int] [Tree]
  deriving Show

-- Label the tree
labelTree :: Tree -> State GlobalState Tree
labelTree Empty = pure Empty
labelTree (Node _ children) = do
  lbl <- nextEnumPosition
  labeledChildren <- descendEnumTree $ mapM labelTree children
  pure $ Node lbl labeledChildren

-- Example tree
exampleTree' :: Tree
exampleTree' =
  Node [] [ Node [] []
          , Node [] [ Node [] [] 
                    , Node [] []
                    ]
          , Node [] [ Node [] [] ]
          ]

-- Run it
main :: IO ()
main = do
  let initialState = GlobalState 0 0 0 0 [0] False False mempty mempty 
      labeled = evalState (labelTree exampleTree') initialState
  print labeled