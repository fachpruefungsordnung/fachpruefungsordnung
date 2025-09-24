module Language.Ltml.HTML.CSS.Color
    ( -- * Table Colors
      tableDarkCell
    , tableActiveRow
    , tableCellBorder

      -- * Link Colors
    , linkText
    , linkTextHover
    , linkUnderline

      -- * Error Colors
    , errorText
    , errorBoxBorder
    ) where

import Clay

tableDarkCell :: Color
tableDarkCell = grayish 225

tableActiveRow :: Color
tableActiveRow = darken 0.2 tableDarkCell

tableCellBorder :: Color
tableCellBorder = grayish 180

-------------------------------------------------------------------------------

linkText :: Color
linkText = rgb 0 0 100

linkTextHover :: Color
linkTextHover = red

linkUnderline :: Color
linkUnderline = red

-------------------------------------------------------------------------------

errorText :: Color
errorText = red

errorBoxBorder :: Color
errorBoxBorder = red
