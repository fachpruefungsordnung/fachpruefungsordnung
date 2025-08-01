{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Language.Ltml.ToLaTeX.LabelState (
  LabelState (..),
  nextSupersection,
  nextSection,
  nextParagraph,
  insertLabel
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
data LabelState = LabelState
  { supersection :: Int             -- counter for supersections
  , section :: Int                  -- counter for sections
  , paragraph :: Int                -- counter for paragraphs within a section
  , onlyOneParagraph :: Bool        -- needed for sections with only one paragraphs
  , isSupersection :: Bool          -- needed for heading
  , identifier :: LaTeX             -- identifier for formatting
  , labelToRef :: Map Label LT.Text -- map for labels
  } deriving Show

nextSupersection :: State LabelState Int
nextSupersection = do
  st <- get
  let n = supersection st + 1
  put st { supersection = n }
  pure n

nextSection :: State LabelState Int
nextSection = do
  st <- get
  let n = section st + 1
  put st { section = n, paragraph = 0 }
  pure n

nextParagraph :: State LabelState Int
nextParagraph = do
  st <- get
  let n = paragraph st + 1
  put st { paragraph = n }
  pure n

insertLabel :: Maybe Label -> LT.Text -> State LabelState ()
insertLabel mLabel ident = do
  maybe (pure ()) (\l -> modify (\s -> s { labelToRef = insert l ident (labelToRef s) })) mLabel

labelSupersection :: Supersection -> State LabelState Supersection
labelSupersection (Supersection _ children) = do
  _ <- nextSupersection
  st <- get
  labeledChildren <- mapM labelSection children
  pure $ Supersection [supersection st] labeledChildren

labelSection :: Section -> State LabelState Section
labelSection (Section _ children) = do
  _ <- nextSection
  st <- get
  labeledChildren <- mapM labelParagraph children
  pure $ Section [supersection st, section st] labeledChildren

labelParagraph :: Paragraph -> State LabelState Paragraph
labelParagraph (Paragraph _) = do
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

-- Run it
main :: IO ()
main = do
  let initialState = LabelState 0 0 0 False False mempty mempty 
      labeled = evalState (labelSupersection exampleTree) initialState
  print labeled