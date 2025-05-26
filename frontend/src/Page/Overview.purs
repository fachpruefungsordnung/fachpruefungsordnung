{-
Overview Page that allows access to the Different Projects
 -}

module FPO.Page.Overview where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List as List
import Data.List.Types (Nil, Cons)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Type.Proxy (Proxy(..))

-- the type of id should be made compatible with the backend once possible.
type ID = Int
type Project = { name :: Maybe String, id :: Maybe ID} 
data Action
  = Initialize
  | LoadProject ID

type State = 
  {projects :: List.List Project}

data Output 
  = OpenProject ID

type Input = Unit

initialState :: State
initialState = { projects = Nil}

overview :: forall query m. MonadEffect m => H.Component query Input Output m
overview = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  }

handleAction :: forall m. Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  --Here, the list of available Projects should be requested from the frontend. For now, two pages are made up.
  Initialize -> do
    H.modify_ \state -> 
      state {projects = Cons {name = Just "Project1", id = Just 1}
                       (Cons {name = Just "Project2", id = Just 2} Nil)}
  
  LoadProject projId -> do
    H.raise (OpenProject projId)