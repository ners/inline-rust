module Submodule.Submodule where

import Data.Int
import Data.Word
import Language.Rust.Inline
import Test.Hspec

extendContext basic
setCrateModule

subsubmoduleTest :: Spec
subsubmoduleTest = describe "Subsubmodules" $ it "Can link against subsubmodules" $ [rust| i32 { 42 } |] `shouldBe` 42
