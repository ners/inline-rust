{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}

module Language.Rust.Inline.Storable.Tuple where

import Control.Monad (join)
import Foreign.Storable
import Language.Rust.Inline.TH.Storable

fmap join (traverse mkTupleStorable [2..16])
