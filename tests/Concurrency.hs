module Concurrency where

import Control.Concurrent.Async
import Control.Monad (replicateM_)
import Foreign (withForeignPtr, peek)
import Language.Rust.Inline
import Language.Rust.Inline.TH (sizeOfWith)
import Test.Hspec
import Data.Word

extendContext foreignPointers
extendContext pointers
extendContext prelude
extendContext basic
setCrateModule

concurrencySpec :: Spec
concurrencySpec = describe "Concurrency" $ do
    it "does not crash" $ do
        let p = [rust| ForeignPtr<u64> { Box::new(0).into() } |]
        replicateConcurrently_ 10000 $ replicateM_ 100 [rustIO| Option<()> { *$(p: &mut u64) += 1; Some(()) } |]
        withForeignPtr p peek `shouldNotReturn` 0
