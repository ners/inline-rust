{-# LANGUAGE CPP #-}

#ifdef darwin_HOST_OS
{-# OPTIONS_GHC -optl-Wl,-all_load #-}
#else
{-# OPTIONS_GHC -optl-Wl,--whole-archive #-}
#endif

module Main where

import Language.Rust.Inline

import AlgebraicDataTypes
import ByteStrings
import Concurrency (concurrencySpec)
import Data.Word
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import ForeignPtr
import FunctionPointerTypes
import GhcUnboxedTypes
import PointerTypes
import PreludeTypes
import SimpleTypes
import Submodule
import Submodule.Submodule
import Test.Hspec

extendContext basic
setCrateRoot []

main :: IO ()
main = hspec $
  describe "Rust quasiquoter" $ do
    algebraicDataTypes
    bytestringSpec
    concurrencySpec
    foreignPtrTypes
    funcPointerTypes
    ghcUnboxedTypes
    pointerTypes
    preludeTypes
    simpleTypes
    submoduleTest
    subsubmoduleTest
