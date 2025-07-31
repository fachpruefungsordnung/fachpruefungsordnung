{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.References (ReferenceType (..), genReference, addLabelToState) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Text (Text)
import Language.Ltml.AST.Label (Label (unLabel))
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.Common
import Lucid

data ReferenceType = SectionRef | ParagraphRef | SentenceRef

-- | Generates fitting german Reference Html based on referenced type.
--   This relies on the GlobalState being set up properly for the referenced scope.
--   (e.g. currentParagraphIDHtml being set)
genReference :: ReferenceType -> HtmlReaderState
genReference ref = do
    readerState <- ask
    case ref of
        SectionRef -> return $ toHtml ("§ " :: Text) <> currentSectionIDHtml readerState
        ParagraphRef ->
            let mParagraphIDText = mCurrentParagraphIDHtml readerState
             in case mParagraphIDText of
                    Nothing ->
                        return $
                            b_
                                [class_ (Class.className Class.FontRed)]
                                "Error: Labeled paragraph does not have any identifier!"
                    Just paragraphIDHtml -> do
                        sectionRef <- genReference SectionRef
                        return $ sectionRef <> toHtml (" Absatz " <> paragraphIDHtml)
        SentenceRef -> do
            globalState <- get
            paragraphRef <- genReference ParagraphRef
            return $ paragraphRef <> toHtml (" Satz " <> show (currentSentenceID globalState))


-- TODO: define Trie Map in GlobalState to track label references

-- | Generates Reference String as Html and adds (Label, Html) pair to GlobalState
--   This function heavily relies on the GlobalState context. 
--   Especially the referenced scope must be evaluated (e.g. the currentSectionIDHtml must be set)
addLabelToState
    :: Label -> ReferenceType -> ReaderT ReaderState (State GlobalState) ()
addLabelToState label ref = do
    referenceHtml <- genReference ref
    modify (\s -> s {labels = (unLabel label, referenceHtml) : labels s})
