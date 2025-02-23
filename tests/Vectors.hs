module Vectors where

import Language.Rust.Inline
import Language.Rust.Inline.TH
import Test.Hspec
import Control.Monad (forM)
import Foreign (withForeignPtr)
import Data.Word
import qualified Foreign

extendContext basic
extendContext prelude
extendContext foreignPointers
extendContext vectors
setCrateModule

vectorsSpec :: Spec
vectorsSpec = describe "Vectors" $ do
    it "can marshal list return values" $ do
        let mints = [rust| Vec<Option<u64>> { vec![Some(17), None] } |]
        mints `shouldBe` [Just 17, Nothing]

        let fps = [rust| Vec<ForeignPtr<u64>> { vec![Box::new(17).into(), Box::new(42).into() ] } |]
        values <- forM fps $ flip withForeignPtr Foreign.peek
        values `shouldBe` [17, 42]

    it "can marshal list arguments" $ do
        let ints = [17, 42] :: [Word64]
        let rsum = [rust| u64 { $(ints: Vec<u64>).iter().sum() } |]
        rsum `shouldBe` sum ints
