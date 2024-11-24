{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module ForeignPtr where

import Language.Rust.Inline

import Data.Word (Word64)
import Foreign (Storable (..))
import Foreign.ForeignPtr
import Foreign.Ptr
import Test.Hspec

extendContext foreignPointers
extendContext basic
setCrateModule

foreignPtrTypes :: Spec
foreignPtrTypes = describe "ForeignPtr types" $ do
    it "Can marshal ForeignPtr arguments" $ do
        p <- mallocForeignPtr
        withForeignPtr p (`poke` 42)
        let read = [rust| u64 { unsafe { *$(p: *const u64) } } |]
        42 `shouldBe` read

    it "Can marshal ForeignPtr arguments as references" $ do
        p <- mallocForeignPtr
        withForeignPtr p (`poke` 42)
        let read =
                [rust| u64 { *$(p: &u64) } |]
        42 `shouldBe` read

    it "Can marshal ForeignPtr arguments as mutable references" $ do
        p <- mallocForeignPtr
        withForeignPtr p (`poke` 42)
        let prev =
                [rust| u64 {
                        let p = $(p: &mut u64);
                        let ret = *p;
                        *p = 43;
                        ret
                    } |]
        prev `shouldBe` 42
        withForeignPtr p peek >>= (`shouldBe` 43)

    it "Can mutate ForeignPtr arguments" $ do
        p <- mallocForeignPtr
        [rustIO| () {
            unsafe { *$(p: *mut u64) = 42; }
        } |]
        val <- withForeignPtr p peek
        val `shouldBe` 42

    it "Can marshal ForeignPtr returns" $ do
        let p = [rust| ForeignPtr<u64> { Box::new(42).into() }|]
        val <- withForeignPtr p peek
        val `shouldBe` 42
