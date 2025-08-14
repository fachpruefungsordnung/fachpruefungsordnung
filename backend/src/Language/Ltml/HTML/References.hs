module Language.Ltml.HTML.References (ReferenceType (..), addMaybeLabelToState) where

import Control.Monad.Reader
import Control.Monad.State
import Language.Ltml.AST.Label (Label (unLabel))
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.FormatString (identifierFormat)
import Lucid

data ReferenceType
    = -- | Reference to a super-section, displayed as "Abschnitt i"
      SuperSectionRef
    | -- | Reference to a section, displayed as "§ i"
      SectionRef
    | -- | Reference to a paragraph in a section, displayed as "§ i Absatz i"
      ParagraphRef
    | -- | Reference to a sentence in a paragraph, displayed as "§ i Absatz i Satz i"
      SentenceRef
    | -- | Reference to a single enum item
      EnumItemRef

-- | Generates fitting reference Html based on referenced type.
--   This relies on the GlobalState being set up properly for the referenced scope.
--   (e.g. currentParagraphIDHtml being set)
genReference
    :: ReferenceType -> ReaderT ReaderState (State GlobalState) (Html ())
genReference ref = do
    readerState <- ask
    case ref of
        SuperSectionRef ->
            return $ currentSuperSectionIDHtml readerState
        SectionRef -> return $ currentSectionIDHtml readerState
        ParagraphRef -> return $ currentParagraphIDHtml readerState
        SentenceRef -> gets (toHtml . show . currentSentenceID)
        EnumItemRef ->
            -- \| Get current item number and enumeration FormatString from state and build corresponding reference
            do
                enumIDFormatString <- asks currentEnumIDFormatString
                enumID <- gets currentEnumItemID
                return $ identifierFormat enumIDFormatString enumID

-- TODO: Maybe? define Trie Map in GlobalState to track label references

-- | If (Just label): generates reference String as Html and adds (Label, Html) pair to GlobalState;
--   else: does nothing;
--   This function heavily relies on the GlobalState context.
--   Especially the referenced scope must be evaluated (e.g. the currentSectionIDHtml must be set)
addMaybeLabelToState
    :: Maybe Label -> ReferenceType -> ReaderT ReaderState (State GlobalState) ()
addMaybeLabelToState mLabel ref = case mLabel of
    Nothing -> return ()
    Just label -> do
        referenceHtml <- genReference ref
        modify (\s -> s {labels = (unLabel label, referenceHtml) : labels s})

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
