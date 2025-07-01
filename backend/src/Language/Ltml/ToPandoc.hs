-- TODO: Rename to ./Internal/Monad.hs

module Language.Ltml.ToPandoc
    ( ToPandoc
    , evalToPandoc
    , getVisibleIdent
    , putVisibleIdent
    , headed
    )
where

import Control.Monad.Reader (Reader, asks, local, runReader)
import Control.Monad.State (StateT, gets, modify, runStateT)
import Data.Map (Map, empty, lookup, insertLookupWithKey)
import Data.Text (Text)
import Language.Ltml.AST.Label (Label)
import Prelude hiding (lookup)
import qualified Text.Pandoc as P (Attr, Block (Header), Inline)

data RContext = RContext
    { rcContext :: Context
    , rcHeadingLevel :: Int
    }

data Context = Context
    { ctxVisibleIdentMap :: Map Label Text
    }

type ToPandoc = StateT Context (Reader RContext)

evalToPandoc :: Int -> ToPandoc a -> a
evalToPandoc headingLevel w = y
  where
    (y, ctx) =
        runReader
            (runStateT w (Context empty))
            (RContext ctx headingLevel)

getVisibleIdent :: Label -> ToPandoc (Maybe Text)
getVisibleIdent lbl = asks (lookup lbl . ctxVisibleIdentMap . rcContext)

putVisibleIdent :: Label -> Text -> ToPandoc ()
putVisibleIdent lbl vi = do
    viMap <- gets ctxVisibleIdentMap
    case insertLookupWithKey (\_ _ _ -> vi) lbl vi viMap of
        (Just _, _) -> error "Re-used label" -- TODO (avoid `error`)
        (Nothing, viMap') -> modify (\ctx -> ctx {ctxVisibleIdentMap = viMap'})

headed
    :: ToPandoc (P.Attr, [P.Inline])
    -> ToPandoc [P.Block]
    -> ToPandoc [P.Block]
headed headingW bodyW = do
    lvl <- asks rcHeadingLevel
    heading <- uncurry (P.Header lvl) <$> headingW
    body <- local (\rc -> rc {rcHeadingLevel = lvl + 1}) bodyW
    return $ heading : body
