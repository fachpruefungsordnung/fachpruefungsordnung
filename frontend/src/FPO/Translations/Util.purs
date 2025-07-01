module FPO.Translations.Util where

import FPO.Data.Store (Store) as Store
import FPO.Translations.Labels (Labels)
import FPO.Translations.Translator (FPOTranslator)
import Halogen.Store.Select (Selector, selectEq)
import Simple.I18n.Translator (Translator)

type FPOState r = { translator :: Translator Labels | r }

selectTranslator :: Selector Store.Store FPOTranslator
selectTranslator = selectEq _.translator
