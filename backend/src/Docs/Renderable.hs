{-# LANGUAGE OverloadedStrings #-}

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

class Renderable r where
    kindOf :: r -> LSD.KindName
    typeOf :: r -> LSD.TypeName
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

data DirectRenderable id
    = DirectRenderable
    { kind :: LSD.KindName
    , type_ :: LSD.TypeName
    , content :: Text
    , identifier :: id
    }

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
