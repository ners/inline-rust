{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -w #-}

module Language.Rust.Inline.Context.ByteString where

import Language.Rust.Inline.Context
import Language.Rust.Inline.TH

import Language.Rust.Data.Ident (Ident (..), mkIdent)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Rust.Quote (ty)
import Language.Rust.Syntax

import Foreign.Storable

import Control.Monad (join, unless)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor (void)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

bytestrings :: Q Context
bytestrings = do
    bytestringT <- [t|ByteString|]
    pure $ Context ([rule], [rev bytestringT], [rustByteString, impl])
  where
    rule rty _
        | rty == void [ty| &[u8] |] = pure ([t|ByteString|], pure . pure $ void [ty| RustByteString |])
        | rty == void [ty| Vec<u8> |] = pure ([t|ByteString|], pure . pure $ void [ty| RustMutByteString |])
    rule _ _ = mempty

    rev _ _ _ = mempty

    rustByteString =
        unlines
            [ "#[repr(C)]"
            , "#[derive(Copy,Clone)]"
            , "pub struct RustByteString(*const u8, usize);"
            , ""
            , "#[repr(C)]"
            , "#[derive(Copy, Clone)]"
            , "pub struct RustMutByteString(*mut u8, usize, extern \"C\" fn (*mut u8, usize) -> ());"
            ]

    impl =
        unlines
            [ "impl<'a> MarshalInto<&'a [u8]> for RustByteString {"
            , "  fn marshal(self) -> &'a [u8] {"
            , "    let RustByteString(ptr, len) = self;"
            , "    unsafe { std::slice::from_raw_parts(ptr, len) }"
            , "  }"
            , "}"
            , ""
            , "impl MarshalInto<RustMutByteString> for Vec<u8> {"
            , "  fn marshal(self) -> RustMutByteString {"
            , "    let bytes = Box::leak(self.into_boxed_slice());"
            , "    let len = bytes.len();"
            , ""
            , "    extern fn free_bytestring(ptr: *mut u8, len: usize) -> () {"
            , "      let bytes = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, len) ) };"
            , "      drop(bytes)"
            , "    }"
            , "    RustMutByteString(bytes.as_mut_ptr(), len, free_bytestring)"
            , "  }"
            , "}"
            ]
