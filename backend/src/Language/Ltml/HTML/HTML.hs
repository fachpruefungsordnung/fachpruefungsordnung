{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.HTML.HTML () where

import Data.ByteString.Lazy
import Data.Text
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
                        ]
                    )
                )
            ]
        )

testHtml :: IO ()
testHtml = renderToFile "static/out.html" (docToHtml testAST)

renderHtml :: Document -> ByteString
renderHtml document = renderBS $ docToHtml document

docToHtml :: Document -> Html ()
docToHtml doc = html_ $ do
    head_ $ do
        title_ "Test Dokument"
        link_ [rel_ "stylesheet", href_ "out.css"]
    body_ $ do
        toHtml doc

instance ToHtml Document where
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

instance ToHtml Section where
    toHtml (Section format heading children) = case children of
        Right cs -> toHtml cs
        Left cs -> toHtml cs

instance ToHtml Paragraph where
    toHtml (Paragraph format textTrees) = toHtml textTrees

instance (ToHtmlStyle style) => ToHtml (TextTree style enum special) where
    toHtml textTree = case textTree of
        Word text -> toHtml text
        Space -> toHtml (" " :: Text)
        Special _ -> toHtml ("Error: Special not supported yet" :: Text)
        Reference label -> toHtml label
        Styled style textTrees -> toHtmlStyle style $ toHtml textTrees
        Enum enum -> toHtml ("Error: Enum not supported yet" :: Text)
        Footnote _ -> toHtml ("Error: FootNotes not supported yet" :: Text)

class ToHtmlStyle style where
    toHtmlStyle :: (Monad m) => style -> (HtmlT m a -> HtmlT m a)

instance ToHtmlStyle FontStyle where
    toHtmlStyle Bold = b_
    toHtmlStyle Italics = i_
    toHtmlStyle Underlined = span_ [class_ Class.underlined]

instance ToHtml Label where
    toHtml label = toHtml $ unLabel label

instance (ToHtml a) => ToHtml [a] where
    toHtml [] = mempty
    toHtml (a : as) = do
        toHtml a
        toHtml as
