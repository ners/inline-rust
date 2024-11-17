{-# LANGUAGE TemplateHaskell, QuasiQuotes, CPP #-}

#ifdef darwin_HOST_OS
{-# OPTIONS_GHC -optl-Wl,-all_load #-}
#else
{-# OPTIONS_GHC -optl-Wl,--whole-archive #-}
#endif

module Main where

import Language.Rust.Inline

import SimpleTypes
import GhcUnboxedTypes
import PointerTypes
import FunctionPointerTypes
import PreludeTypes
import AlgebraicDataTypes
import Data.Word
import Test.Hspec
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array
extendContext basic
setCrateRoot []

main :: IO ()
main = hspec $
  describe "Rust quasiquoter" $ do
    simpleTypes
    ghcUnboxedTypes
    pointerTypes
    funcPointerTypes
    preludeTypes
    algebraicDataTypes

