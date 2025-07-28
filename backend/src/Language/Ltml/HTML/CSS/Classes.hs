{-# LANGUAGE OverloadedStrings #-}
-- Turn incomplete pattern matches into error, so that every defined Class has to have a style
-- This ensures that every class used in Lucid also has an entry in the css stylesheet
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

module Language.Ltml.HTML.CSS.Classes (Class (..), className, classStyle, enumLevel) where

import Clay hiding (Content, stringContent)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Language.Ltml.HTML.CSS.CustomClay

data Class
    = -- | Underlining basic text
      Underlined
    | -- | Enum with 1., 2., 3., ...
      EnumNum
    | -- | Enum with a), b), c), ...
      EnumCharPar
    | -- | Enum with aa), bb), cc), ...
      EnumCharCharPar
    | -- | Enum for errors
      EnumFail
    deriving (Eq, Enum, Bounded)

-- | Returns the html class name of given Class
className :: Class -> Text
className Underlined = "underlined"
className EnumNum = "enumNum"
className EnumCharPar = "enumCharPar"
className EnumCharCharPar = "enumCharCharPar"
className EnumFail = "enumFail"

-- | maps Class to its css style definition
classStyle :: Class -> Css
classStyle Underlined = toClassSelector Underlined ? textDecoration underline
classStyle EnumNum =
    enumCounter
        (className EnumNum)
        (stringCounter "(" <> counterNum "item" <> stringCounter ") ")
classStyle EnumCharPar =
    enumCounter (className EnumCharPar) (counterChar "item" <> stringCounter ") ")
classStyle EnumCharCharPar =
    enumCounter
        (className EnumCharCharPar)
        (counterChar "item" <> counterChar "item" <> stringCounter ") ")
classStyle EnumFail = enumCounter (className EnumFail) (stringCounter "x. ")

-- | converts Class to Clay Selector and adds "." infront for css selection
toClassSelector :: Class -> Selector
toClassSelector c = fromString ("." ++ unpack (className c))

-----------------------------------------------------------------------------------------

-- | Example Enumertion Levels for an FPO
enumLevel :: Int -> Text
enumLevel i = className $ case i of
    0 -> EnumNum
    1 -> EnumCharPar
    2 -> EnumCharCharPar
    _ -> EnumFail -- dont throw error but place placeholder symbol

-- | Builds CSS class with specfied counter for ordered lists
enumCounter :: Text -> Counter -> Css
enumCounter enumClassName counterContent = do
    ol # byClass enumClassName ? do
        counterReset "item"

    ol # byClass enumClassName |> li ? do
        display block

    ol # byClass enumClassName |> li ? before & do
        counter counterContent
        counterIncrement "item"