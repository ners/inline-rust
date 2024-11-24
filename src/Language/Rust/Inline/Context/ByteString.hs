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

import Debug.Trace (traceM)

bytestrings :: Q Context
bytestrings =
    pure $ Context ([rule], [], [rustByteString, impl])
  where
    rule rty _
        | rty == void [ty| &[u8] |] = pure ([t|ByteString|], pure . pure $ void [ty| RustByteString |])
        | rty == void [ty| Vec<u8> |] = pure ([t|ByteString|], pure . pure $ void [ty| RustOwnedByteString |])
        | rty == void [ty| Option<Vec<u8>> |] = pure ([t|Maybe ByteString|], pure . pure $ void [ty| RustOwnedByteString |])
    rule _ _ = mempty

    rustByteString =
        unlines
            [ "#[repr(C)]"
            , "#[derive(Copy,Clone)]"
            , "pub struct RustByteString(*const u8, usize);"
            , ""
            , "#[repr(C)]"
            , "#[derive(Copy, Clone)]"
            , "pub struct RustOwnedByteString(*mut u8, usize, extern \"C\" fn (*mut u8, usize));"
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
            , "impl MarshalInto<RustOwnedByteString> for Vec<u8> {"
            , "  fn marshal(self) -> RustOwnedByteString {"
            , "    let bytes = Box::leak(self.into_boxed_slice());"
            , "    let len = bytes.len();"
            , ""
            , "    extern fn free(ptr: *mut u8, len: usize) {"
            , "      let bytes = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, len) ) };"
            , "      drop(bytes);"
            , "    }"
            , "    RustOwnedByteString(bytes.as_mut_ptr(), len, free)"
            , "  }"
            , "}"
            , ""
            , "impl MarshalInto<RustOwnedByteString> for Option<Vec<u8>> {"
            , "  fn marshal(self) -> RustOwnedByteString {"
            , "    extern fn panic(ptr: *mut u8, len: usize) {"
            , "      panic!(\"Attempted to free a null ByteString\");"
            , "    }"
            , "    self.map(|bs| bs.marshal()).unwrap_or(RustOwnedByteString(std::ptr::null_mut(), 0, panic))"
            , "  }"
            , "}"
            ]
