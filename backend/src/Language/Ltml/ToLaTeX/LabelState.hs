{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Language.Ltml.ToLaTeX.LabelState (
  LabelState (..),
  nextSupersection,
  nextSection,
  nextParagraph
) where

import Control.Monad.State

-- example structure
data Supersection = Supersection [Int] [Section]
  deriving Show

data Section = Section [Int] [Paragraph]
  deriving Show

newtype Paragraph = Paragraph [Int]
  deriving Show

-- State for labeling
data LabelState = LabelState
  { supersection :: Int
  , section :: Int
  , paragraph :: Int
  , onlyOneParagraph :: Bool
  , isSupersection :: Bool
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
  let initialState = LabelState 0 0 0 False False
      labeled = evalState (labelSupersection exampleTree) initialState
  print labeled