{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Docs.Renderable
-- Description : Obtaining Information Needed for Rendering
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module provides a class to obtain information from text nodes needed to
-- render these text nodes as html, pdf or the like.
module Docs.Renderable
    ( Renderable (..)
    , DirectRenderable (..)
    , directRenderable
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Docs.TextElement as TextElement
import Docs.TextRevision (TextElementRevision)
import qualified Docs.TextRevision as TextRevision
import qualified Language.Lsd.AST.Common as LSD

-- | Class for all types renerable as a text element.
class Renderable r where
    -- | The kind of the text
    kindOf :: r -> LSD.KindName

    -- | The type of the text
    typeOf :: r -> LSD.TypeName

    -- | The content of the text; the text itself
    contentOf :: r -> Text

instance Renderable TextElementRevision where
    kindOf =
        LSD.KindName
            . Text.unpack
            . TextElement.textElementKind
            . TextRevision.textElement

    typeOf =
        LSD.TypeName
            . Text.unpack
            . TextElement.textElementType
            . TextRevision.textElement

    contentOf =
        maybe "" TextRevision.content . TextRevision.revision

-- | Datatype only intended to be used as a @Renderable@.
-- Can be used to enrich not-@Renderable@ types such as @Text@ with informations
-- needed to be renderable
data DirectRenderable id
    = DirectRenderable
    { kind :: LSD.KindName
    , type_ :: LSD.TypeName
    , content :: Text
    , identifier :: id
    }

-- | Make a @DirectRenderable@ from any @Renderable@
directRenderable :: (Renderable r) => r -> id -> DirectRenderable id
directRenderable element id_ =
    DirectRenderable
        { kind = kindOf element
        , type_ = typeOf element
        , content = contentOf element
        , identifier = id_
        }

instance Renderable (DirectRenderable id) where
    kindOf = kind
    typeOf = type_
    contentOf = content
