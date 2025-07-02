module Language.Ltml.ToPandoc
    ( Context
    , ToPandoc
    )
where

import Control.Monad.Reader (Reader)
import Data.Map (Map)
import Data.Text (Text)
import Language.Ltml.AST.Label (Label)

type Context = Map Label Text

type ToPandoc = Reader Context
