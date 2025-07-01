-- | The AppM monad is our main application monad, which combines the Halogen monad with
-- | the Store monad. It allows us to manage the state of the application and perform
-- | actions on it.
-- |
-- | As of now, the AppM monad is only used for navigating between different pages in the
-- | application. However, it can be extended to include other functionalities as needed
-- | (e.g. logging, communication with the backend, resource management, etc.).

module FPO.AppM where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FPO.Data.Navigate (class Navigate)
import FPO.Data.Route as Route
import FPO.Data.Store as Store
import Halogen (liftEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT)
import Routing.Duplex as RD
import Routing.Hash (setHash)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM
  :: forall query input output
   . Store.Store
  -> H.Component query input output AppM
  -> Aff (H.Component query input output Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< RD.print Route.routeCodec
