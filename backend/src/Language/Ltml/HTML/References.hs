module Language.Ltml.HTML.References
    ( -- * Building References
      addMaybeLabelToState
    , buildEnumItemRefHtml
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Language.Ltml.AST.Label (Label)
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.FormatString (identifierFormat)
import Lucid

-- TODO: Maybe? define Trie Map in GlobalState to track label references

-- | If (Just label): adds (Label, Html) pair to GlobalState;
--   else: does nothing;
addMaybeLabelToState
    :: Maybe Label -> Html () -> ReaderT ReaderState (State GlobalState) ()
addMaybeLabelToState mLabel referenceHtml = case mLabel of
    Nothing -> return ()
    Just label -> modify (\s -> s {labels = (label, referenceHtml) : labels s})

-- | Builds enum item reference using the EnumFormat from the ReaderState
buildEnumItemRefHtml :: Int -> ReaderT ReaderState (State GlobalState) (Html ())
buildEnumItemRefHtml enumItemID = do
    -- \| Get current item number and enumeration FormatString from state and build corresponding reference
    enumIDFormatString <- asks currentEnumIDFormatString
    return $ identifierFormat enumIDFormatString enumItemID

-------------------------------------------------------------------------------

-- | Old version that allows full references with a single label (e.g. "§ 3 Absatz 4 Satz 2")
-- genReference
--     :: ReferenceType -> ReaderT ReaderState (State GlobalState) (Html ())
-- genReference ref = do
--     readerState <- ask
--     case ref of
--         SuperSectionRef ->
--             return $ toHtml ("Abschnitt " :: Text) <> currentSuperSectionIDHtml readerState
--         SectionRef -> return $ toHtml ("§ " :: Text) <> currentSectionIDHtml readerState
--         ParagraphRef ->
--             let mParagraphIDText = mCurrentParagraphIDHtml readerState
--              in case mParagraphIDText of
--                     Nothing ->
--                         return $
--                             b_ <#> Class.FontRed $
--                                 "Error: Labeled paragraph does not have any identifier!"
--                     Just paragraphIDHtml -> do
--                         sectionRef <- genReference SectionRef
--                         return $ sectionRef <> (toHtml (" Absatz " :: Text) <> paragraphIDHtml)
--         SentenceRef -> do
--             globalState <- get
--             paragraphRef <- genReference ParagraphRef
--             return $
--                 paragraphRef <> toHtml (" Satz " <> show (currentSentenceID globalState))
--         EnumItemRef -> do
--             globalState <- get
--             enumNestingLvl <- asks enumNestingLevel
--             -- | Use the enumeration nesting level of the parent enumeration
--             --   (that this enum item lives in) by subtracting 1
--             return $ toHtml $ Class.enumIdentifier (Class.enumLevel (enumNestingLvl - 1)) (currentEnumItemID globalState)
