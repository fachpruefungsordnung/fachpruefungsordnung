module Translations.Util where

import FPO.Data.Store (Store) as Store
import Halogen.Store.Select (Selector, selectEq)
import Simple.I18n.Translator (Translator)
import Translations.Labels (Labels)
import Translations.Translator (EqTranslator)

type FPOState r = { translator :: Translator Labels | r }

selectTranslator :: Selector Store.Store EqTranslator
selectTranslator = selectEq _.translator
