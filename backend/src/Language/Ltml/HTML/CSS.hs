module Language.Ltml.HTML.CSS (writeCss, mainStylesheet) where

import Clay hiding (map)

import Data.Text.Lazy.IO (writeFile)
import Language.Ltml.HTML.CSS.Classes
import Language.Ltml.HTML.CSS.Util (buildCssCounters)
import Language.Ltml.HTML.Common (EnumStyleMap)
import Prelude hiding (writeFile)

writeCss :: Css -> FilePath -> IO ()
writeCss css path = writeFile path (render css)

-- | List of all Css Classes defined in Language.Ltml.HTML.CSS.Classes
cssClasses :: [Css]
cssClasses = map classStyle [minBound .. maxBound]

-- | Builds CSS Counters and produces final main stylesheet
mainStylesheet :: EnumStyleMap -> Css
mainStylesheet enumStyleMap = mconcat (buildCssCounters enumStyleMap : cssClasses)
