module Language.Ltml.HTML.CSS.Color
    ( -- * Table Colors
      tocDarkCell
    , tocActiveRow
    , tocCellBorder
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

tocDarkCell :: Color
tocDarkCell = grayish 225

tocActiveRow :: Color
tocActiveRow = darken 0.2 tocDarkCell

tocCellBorder :: Color
tocCellBorder = grayish 180

tableCellBorder :: Color
tableCellBorder = grayish 40

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
