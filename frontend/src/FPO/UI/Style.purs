-- | This module contains some additional styling helpers and goodies
-- | for the FPO app, useful for creating and reusing CSS styles across
-- | components and pages.

module FPO.UI.Style where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

-- | Popover attribute for tooltips. Can be used just like any other HTML
-- | attribute, and adds a tooltip to the element when hovered.
-- |
-- | Example usage:
-- |    `HH.div [ popover "Tooltip text" ] [ HH.text "Hover me!" ]`
popover :: forall r i. String -> HH.IProp r i
popover = HH.attr (HH.AttrName "data-tooltip")

-- | Class for responsive buttons that adapt to different screen sizes.
-- | This class can be used to ensure buttons look good on both mobile
-- | and desktop views.
responsiveButton :: HH.ClassName
responsiveButton = HH.ClassName "responsive-btn"

-- | A cyan style button, predominantly used as main-feature buttons
-- | of some admin pages.
cyanStyle :: forall r i. HH.IProp (class :: String | r) i
cyanStyle = HP.classes
  [ HB.btn
  , HB.btnOutlineInfo
  , HB.btnLg
  , HB.p3
  , HB.textDark
  , responsiveButton
  ]
