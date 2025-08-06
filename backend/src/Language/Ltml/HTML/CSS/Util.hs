module Language.Ltml.HTML.CSS.Util ((<#>), cssClass_) where

import Language.Ltml.HTML.CSS.Classes (Class, className)
import Lucid (Attributes, class_)

-- | Constructs HTML element with given Class
(<#>) :: ([Attributes] -> a) -> Class -> a
htmlFunc <#> cssClass = htmlFunc [class_ (className cssClass)]

-- | Convert CSS Class to Lucid HTML Attribute
cssClass_ :: Class -> Attributes
cssClass_ = class_ . className
