{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.HTML.HTML () where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Lucid

import Control.Monad.Reader
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Document
import Language.Lsd.AST.Type.Paragraph
import Language.Lsd.AST.Type.Section
import Language.Ltml.AST.Document
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.HTML.CSS.Classes (enumLevel)
import qualified Language.Ltml.HTML.CSS.Classes as Class

testAST :: Document
testAST =
    Document
        DocumentFormat
        DocumentHeader
        ( DocumentBody
            [ Node
                Nothing
                ( Section
                    (SectionFormat (FormatString []))
                    (Heading (FormatString []) [])
                    ( Left
                        [ Node
                            (Just (Label "label"))
                            ( Paragraph
                                (ParagraphFormat (FormatString []))
                                [ Word "Das ist im Paragraph"
                                , Word "Ohne Space"
                                , Space
                                , Word "Nach dem Space"
                                , Word "\n"
                                , Word "Neue Zeile?"
                                , Styled Bold [Word "Bold"]
                                , Styled Italics [Word "Italic"]
                                , Styled Underlined [Word "Underlined"]
                                ]
                            )
                        , Node
                            (Just (Label "Zweite Node Label"))
                            ( Paragraph
                                (ParagraphFormat (FormatString []))
                                [ Enum
                                    ( Enumeration
                                        [ EnumItem [Word "Erstes Item"]
                                        , EnumItem [Word "Zweites Item 1", Styled Bold [Word "Zweites Item 2"]]
                                        , EnumItem
                                            [ Enum
                                                ( Enumeration [EnumItem [Word "Zweite Aufzählung 1"], EnumItem [Word "Zweite 2"]]
                                                )
                                            ]
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )
            ]
        )

testHtml :: IO ()
testHtml = writeFile "static/out.html" (unpack $ renderText $ docToHtml testAST)

renderHtml :: Document -> ByteString
renderHtml document = renderBS $ docToHtml document

docToHtml :: Document -> Html ()
docToHtml doc = html_ $ do
    head_ $ do
        title_ "Test Dokument"
        link_ [rel_ "stylesheet", href_ "out.css"]
    body_ $ do
        runReader (toHtmlM doc) 0

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

-- instance (ToHtml EnumItem) => ToHtml Enumeration where
--     toHtml (Enumeration enumItems) = ol_ [class_ (Class.className Class.EnumNum)] $ foldr ((>>) . li_ . toHtml) mempty enumItems

class ToHtmlM a where
    toHtmlM :: a -> Reader Int (Html ())

instance ToHtmlM Enumeration where
    toHtmlM (Enumeration enumItems) = do
        level <- ask
        nested <- mapM (local (+ 1) . toHtmlM) enumItems
        return $ ol_ [class_ $ enumLevel level] $ foldr ((>>) . li_) mempty nested

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
