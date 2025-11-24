module Test.Components.Splitview where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Components.Splitview" do
  it "updates ratios when updating the record" do

    let initial = { sidebarRatio: 0.2, previewRatio: 0.4, editorRatio: 0.4 }
    let new = initial { sidebarRatio = 0.3, previewRatio = 0.3 }
    new.editorRatio `shouldEqual` 0.4

