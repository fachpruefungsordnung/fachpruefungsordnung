module Language.Ltml.ToPandoc.Label
    ( mLabelW
    )
where

import Data.Text (Text)
import Language.Ltml.AST.Label (Label (unLabel))
import Language.Ltml.ToPandoc (ToPandoc)

-- TODO: Also get visible output identifier, and save that in state.

-- | Get output label from input label.
mLabelW :: Maybe Label -> ToPandoc Text
mLabelW = return . maybe mempty unLabel
