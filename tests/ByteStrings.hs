{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ByteStrings where

import Language.Rust.Inline
import Test.Hspec

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import Data.String

extendContext basic
extendContext bytestrings
setCrateModule

bytestringSpec :: Spec
bytestringSpec = describe "ByteStrings" $ do
    it "can marshal ByteString arguments" $ do
        let inputs = ByteString.pack [0, 1, 2, 3]
            rustSum =
                [rust| u8 {
            let inputs = $( inputs: &[u8] );
            inputs.iter().sum()
        } |]
        rustSum `shouldBe` sum (ByteString.unpack inputs)

    it "can marshal ByteString return values" $ do
        let rustBs =
                [rust| Vec<u8> {
                    vec![0, 1, 2, 3]
                } |]
        ByteString.pack [0, 1, 2, 3]
            `shouldBe` rustBs
        ByteString.unsafeFinalize rustBs
