{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.HTML.HTML () where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Lucid

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
import qualified Language.Ltml.HTML.CSS.ClassNames as Class

testAST :: Document
testAST =
    Document
        DocumentFormat
        DocumentHeader
        ( DocumentBody
            [ Node
                (Just (Label "Heading 1"))
                ( Section
                    (SectionFormat (FormatString []))
                    (Heading (FormatString []) [])
                    ( Left
                        [ Node
                            (Just (Label "Erste Node Label"))
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
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )
            ]
        )

testHtml :: (ToHtml EnumItem) => IO ()
testHtml = renderToFile "static/out.html" (docToHtml testAST)

renderHtml :: (ToHtml EnumItem) => Document -> ByteString
renderHtml document = renderBS $ docToHtml document

docToHtml :: (ToHtml EnumItem) => Document -> Html ()
docToHtml doc = html_ $ do
    head_ $ do
        title_ "Test Dokument"
        link_ [rel_ "stylesheet", href_ "out.css"]
    body_ $ do
        toHtml doc

instance (ToHtml EnumItem) => ToHtml Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtml (Document format header body) = case body of
        DocumentBody [] -> return ()
        DocumentBody nodes -> toHtml nodes

instance (ToHtml a) => ToHtml (Node a) where
    toHtml (Node maybeLabel a) = case maybeLabel of
        Nothing -> toHtml a
        Just label -> do
            h2_ (toHtml label)
            toHtml a

instance (ToHtml EnumItem) => ToHtml Section where
    toHtml (Section format heading children) = case children of
        Right cs -> toHtml cs
        Left cs -> toHtml cs

instance (ToHtml EnumItem) => ToHtml Paragraph where
    toHtml (Paragraph format textTrees) = toHtml textTrees

instance (ToHtmlStyle style, ToHtml enum) => ToHtml (TextTree style enum special) where
    toHtml textTree = case textTree of
        Word text -> toHtml text
        Space -> toHtml (" " :: Text)
        Special _ -> toHtml ("Error: Special not supported yet" :: Text)
        Reference label -> toHtml label
        Styled style textTrees -> toHtmlStyle style $ toHtml textTrees
        Enum enum -> toHtml enum
        Footnote _ -> toHtml ("Error: FootNotes not supported yet" :: Text)

class ToHtmlStyle style where
    toHtmlStyle :: (Monad m) => style -> (HtmlT m a -> HtmlT m a)

instance ToHtmlStyle FontStyle where
    toHtmlStyle Bold = b_
    toHtmlStyle Italics = i_
    toHtmlStyle Underlined = span_ [class_ Class.underlined]

instance (ToHtml EnumItem) => ToHtml Enumeration where
    toHtml (Enumeration enumItems) = ol_ $ foldr ((>>) . li_ . toHtml) mempty enumItems

instance ToHtml EnumItem where
    toHtml (EnumItem textTree) = toHtml textTree

instance ToHtml Label where
    toHtml label = toHtml $ unLabel label

instance (ToHtml a) => ToHtml [a] where
    toHtml [] = mempty
    toHtml (a : as) = do
        toHtml a
        toHtml as
