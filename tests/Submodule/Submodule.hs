{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Submodule.Submodule where

import Data.Int
import Data.Word
import Language.Rust.Inline
import Test.Hspec

extendContext basic
setCrateModule

submoduleTest :: Spec
submoduleTest = describe "Submodules" $ it "Can link against submodules" $ [rust| i32 { 42 } |] `shouldBe` 42
