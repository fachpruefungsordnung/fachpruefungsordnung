{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Ltml.HTML.HTML (renderHtml, docToHtml, sectionToHtml) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Lucid

import Control.Monad.Reader
import Language.Ltml.AST.Document
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.HTML.CSS.Classes (enumLevel)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Lsd.AST.Format (HeadingFormat, FormatString (..))

renderHtml :: Document -> ByteString
renderHtml document = renderBS $ docToHtml document

docToHtml :: Document -> Html ()
docToHtml = aToHtml

sectionToHtml :: Node Section -> Html ()
sectionToHtml = aToHtml

-- | Internal function that creates final HTML wrapper and header
aToHtml :: (ToHtmlM a) => a -> Html ()
aToHtml a = html_ $ do
    head_ $ do
        title_ "Test Dokument"
        link_ [rel_ "stylesheet", href_ "out.css"]
    body_ $ do
        runReader (toHtmlM a) (State {enumNestingLevel = 0})

newtype State = State
    { enumNestingLevel :: Int
    }

class ToHtmlM a where
    toHtmlM :: a -> Reader State (Html ())

instance ToHtmlM Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtmlM (Document format header body) = case body of
        DocumentBody [] -> return $ return ()
        DocumentBody nodes -> toHtmlM nodes

instance (ToHtmlM a) => ToHtmlM (Node a) where
    toHtmlM (Node maybeLabel a) = case maybeLabel of
        Nothing -> toHtmlM a
        Just label -> do
            labelHtml <- b_ <$> toHtmlM label
            aHtml <- toHtmlM a
            return (labelHtml <> aHtml)

instance ToHtmlM Section where
    toHtmlM (Section format heading children) = case children of
        Right cs -> toHtmlM cs
        Left cs -> toHtmlM cs

instance ToHtmlM Heading where
    toHtmlM (Heading format textTree) = undefined 

-- instance ToHtmlFormat HeadingFormat where
--     toHtmlFormat :: HeadingFormat -> (Int, Text) -> Html ()
--     toHtmlFormat (FormatString as) (id, text) = case as of
--         [] -> h2_ ""
        
-- -- | Generates function that builds needed text based on some input
-- --   For example: id and heading text -> h2_ <heading with id> 
-- class ToHtmlFormat format where
--     toHtmlFormat :: format -> a -> Html ()

instance ToHtmlM Paragraph where
    toHtmlM (Paragraph format textTrees) = toHtmlM textTrees


instance (ToHtmlStyle style, ToHtmlM enum) => ToHtmlM (TextTree style enum special) where
    toHtmlM textTree = case textTree of
        Word text -> return $ toHtml text
        Space -> return $ toHtml (" " :: Text)
        Special _ -> return $ toHtml ("Error: Special not supported yet" :: Text)
        Reference label -> toHtmlM label
        Styled style textTrees -> toHtmlStyle style <$> toHtmlM textTrees
        Enum enum -> toHtmlM enum
        Footnote _ -> return $ toHtml ("Error: FootNotes not supported yet" :: Text)

class ToHtmlStyle style where
    toHtmlStyle :: (Monad m) => style -> (HtmlT m a -> HtmlT m a)

instance ToHtmlStyle FontStyle where
    toHtmlStyle Bold = b_
    toHtmlStyle Italics = i_
    toHtmlStyle Underlined = span_ [class_ (Class.className Class.Underlined)]

instance ToHtmlM Enumeration where
    toHtmlM (Enumeration enumItems) = do
        state <- ask
        nested <-
            mapM
                (local (\s -> s {enumNestingLevel = enumNestingLevel s + 1}) . toHtmlM)
                enumItems
        return $
            ol_ [class_ $ enumLevel (enumNestingLevel state)] $
                foldr ((>>) . li_) mempty nested

instance ToHtmlM EnumItem where
    toHtmlM (EnumItem textTrees) = toHtmlM textTrees

instance ToHtmlM Label where
    toHtmlM label = return $ toHtml $ unLabel label

instance (ToHtmlM a) => ToHtmlM [a] where
    toHtmlM [] = return mempty
    toHtmlM (a : as) = do
        aHtml <- toHtmlM a
        asHtml <- toHtmlM as
        return (aHtml <> asHtml)
