{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Rust.Inline.Context where

import Language.Rust.Inline.Pretty (renderType)

import Language.Rust.Quote (ty)
import Language.Rust.Syntax (
    Abi (..),
    Arg (..),
    FnDecl (..),
    Mutability (..),
    Path (..),
    PathParameters (..),
    PathSegment (..),
    Ty (..),
    Unsafety (..),
 )

import Language.Haskell.TH

import Control.Monad (void)
import Data.List (intercalate)
import Data.Monoid (First (..))
import Data.Traversable (for)
import Data.Typeable (Typeable)

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types -- pretty much every type here is used
import Foreign.Ptr (FunPtr, Ptr)

import Foreign.ForeignPtr (ForeignPtr)
import GHC.Exts (
    ByteArray#,
    Char#,
    Double#,
    Float#,
    Int#,
    Word#,
 )

-- Easier on the eyes
type RType = Ty ()
type HType = Type

{- | Represents a prioritized set of rules for mapping Haskell types into Rust
ones and vice versa.

The 'Context' argument encodes the fact that we may need look
recursively into the 'Context' again before possibly producing a Haskell
type.
-}
newtype Context
    = Context
        ( [RType -> Context -> First (Q HType, Maybe (Q RType))]
        , -- Given a Rust type in a quasiquote, we need to look up the
          -- corresponding Haskell type (for the FFI import) as well as the
          -- C-compatible Rust type (if the initial Rust type isn't already
          -- @#[repr(C)]@.

          [HType -> Context -> First (Q RType)]
        , -- Given a field in a Haskell ADT, we need to figure out which
          -- (not-necessarily @#[repr(C)]@) Rust type normally maps into this
          -- Haskell type.

          [String]
          -- Source for the trait impls of @MarshalTo@
        )
    deriving (Semigroup, Monoid, Typeable)

{- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
If the Rust type is not C-compatible, also return a C compatible type. It is
expected that:

  1. The Haskell type have a 'Storable' instance
  2. The C-compatible Rust type have the same layout
-}
lookupRTypeInContext :: RType -> Context -> First (Q HType, Maybe (Q RType))
lookupRTypeInContext rustType context@(Context (rules, _, _)) =
    foldMap (\fits -> fits rustType context) rules

{- | Search in a 'Context' for the Rust type corresponding to a Haskell type.
Looking up the Rust type using 'lookupRTypeInContext' should yield the
initial Haskell type again.
-}
lookupHTypeInContext :: HType -> Context -> First (Q RType)
lookupHTypeInContext haskType context@(Context (_, rules, _)) =
    foldMap (\fits -> fits haskType context) rules

{- | Partial version of 'lookupRTypeInContext' that fails with an error message
if the type is not convertible.
-}
getRTypeInContext :: RType -> Context -> (Q HType, Maybe (Q RType))
getRTypeInContext rustType context =
    case getFirst (lookupRTypeInContext rustType context) of
        Just found -> found
        Nothing ->
            ( fail $
                unwords
                    [ "Could not find information about"
                    , renderType rustType
                    , "in the context"
                    ]
            , Nothing
            )

{- | Partial version of 'lookupHTypeInContext' that fails with an error message
if the type is not convertible.
-}
getHTypeInContext :: HType -> Context -> Q RType
getHTypeInContext haskType context =
    case getFirst (lookupHTypeInContext haskType context) of
        Just found -> found
        Nothing ->
            fail $
                unwords
                    [ "Could not find information about"
                    , pprint haskType
                    , "in the context"
                    ]

{- | Make a 'Context' consisting of rules to map the Rust types on the left to
the Haskell types on the right. The Rust types should all be @#[repr(C)]@
and the Haskell types should all be 'Storable'.
-}
mkContext :: [(Ty a, Q HType, Bool)] -> Q Context
mkContext tys = do
    tys' <- traverse (\(rt, qht, mkImpl) -> do ht <- qht; pure (void rt, ht, mkImpl)) tys
    pure
        ( Context
            ( map fits tys'
            , map rev tys'
            , map impl tys'
            )
        )
  where
    fits (rts, hts, _) rt _
        | rt == rts = pure (pure hts, Nothing)
        | otherwise = mempty

    rev (rts, hts, _) ht _
        | ht == hts = pure (pure rts)
        | otherwise = mempty

    impl (rts, _, mkImpl)
        | mkImpl = implMarshalInto rts
        | otherwise = mempty

-- | Make a default @MarshalInto@ trait impl. (An identity impl)
implMarshalInto :: Ty () -> String
implMarshalInto t =
    unlines
        [ "impl MarshalInto<" ++ tyStr ++ "> for " ++ tyStr ++ " {"
        , "  fn marshal(self) -> " ++ tyStr ++ " { self }"
        , "}"
        ]
  where
    tyStr = renderType t

{- | Make a singleton 'Context' consisting of a rule to map the given Rust type
to the given Haskell type.
-}
singleton :: Ty a -> Q HType -> Q Context
singleton rts qht = mkContext [(rts, qht, True)]

-- * Some handy contexts

-- | Types defined in 'Foreign.C.Types' and the 'std::ffi' module.
ffi :: Q Context
ffi =
    mkContext
        [ ([ty| std::ffi::c_char      |], [t|CChar|], False)
        , ([ty| std::ffi::c_double    |], [t|CDouble|], False)
        , ([ty| std::ffi::c_float     |], [t|CFloat|], False)
        , ([ty| std::ffi::c_int       |], [t|CInt|], False)
        , ([ty| std::ffi::c_long      |], [t|CLong|], False)
        , ([ty| std::ffi::c_longlong  |], [t|CLLong|], False)
        , ([ty| std::ffi::c_schar     |], [t|CSChar|], False)
        , ([ty| std::ffi::c_short     |], [t|CShort|], False)
        , ([ty| std::ffi::c_uchar     |], [t|CUChar|], False)
        , ([ty| std::ffi::c_uint      |], [t|CUInt|], False)
        , ([ty| std::ffi::c_ulong     |], [t|CULong|], False)
        , ([ty| std::ffi::c_ulonglong |], [t|CULLong|], False)
        , ([ty| std::ffi::c_ushort    |], [t|CUShort|], False)
        ]

{- | Types defined in 'Foreign.C.Types' and the 'libc' crate.

There should be no conversion required here - these have /identical/ memory
layouts (since they both promise to have the same memory layout as C) and are
passed on the stack.
-}
libc :: Q Context
libc =
    mkContext
        [ ([ty| libc::c_char      |], [t|CChar|], False) -- char
        , ([ty| libc::c_schar     |], [t|CSChar|], False) -- signed char
        , ([ty| libc::c_uchar     |], [t|CUChar|], False) -- unsigned char
        , ([ty| libc::c_short     |], [t|CShort|], False) -- short
        , ([ty| libc::c_ushort    |], [t|CUShort|], False) -- unsigned short
        , ([ty| libc::c_int       |], [t|CInt|], False) -- int
        , ([ty| libc::c_uint      |], [t|CUInt|], False) -- unsigned int
        , ([ty| libc::c_long      |], [t|CLong|], False) -- long
        , ([ty| libc::c_ulong     |], [t|CULong|], False) -- unsigned long
        , ([ty| libc::ptrdiff_t   |], [t|CPtrdiff|], False) -- ptrdiff_t
        , ([ty| libc::size_t      |], [t|CSize|], False) -- size_t
        , ([ty| libc::wchar_t     |], [t|CWchar|], False) -- wchar_t
        , ([ty| libc::c_longlong  |], [t|CLLong|], False) -- long long
        , ([ty| libc::c_ulonglong |], [t|CULLong|], False) -- unsigned long long
        , ([ty| libc::boolean_t   |], [t|CBool|], False) -- bool
        , ([ty| libc::intptr_t    |], [t|CIntPtr|], False) -- intptr_t
        , ([ty| libc::uintptr_t   |], [t|CUIntPtr|], False) -- uintptr_t
        , ([ty| libc::intmax_t    |], [t|CIntMax|], False) -- intmax_t
        , ([ty| libc::uintmax_t   |], [t|CUIntMax|], False) -- unsigned intmax_t
        , ([ty| libc::clock_t     |], [t|CClock|], False) -- clock_t
        , ([ty| libc::time_t      |], [t|CTime|], False) -- time_t
        , ([ty| libc::useconds_t  |], [t|CUSeconds|], False) -- useconds_t
        , ([ty| libc::suseconds_t |], [t|CSUSeconds|], False) -- suseconds_t
        , ([ty| libc::c_float     |], [t|CFloat|], False) -- float
        , ([ty| libc::c_double    |], [t|CDouble|], False) -- double
        , ([ty| libc::FILE        |], [t|CFile|], True) -- FILE
        , ([ty| libc::fpos_t      |], [t|CFpos|], True) -- fpos_t
        , ([ty| libc::int8_t      |], [t|Int8|], False) -- int8_t
        , ([ty| libc::int16_t     |], [t|Int16|], False) -- int16_t
        , ([ty| libc::int32_t     |], [t|Int32|], False) -- int32_t
        , ([ty| libc::int64_t     |], [t|Int64|], False) -- int64_t
        , ([ty| libc::uint8_t     |], [t|Word8|], False) -- uint8_t
        , ([ty| libc::uint16_t    |], [t|Word16|], False) -- uint16_t
        , ([ty| libc::uint32_t    |], [t|Word32|], False) -- uint32_t
        , ([ty| libc::uint64_t    |], [t|Word64|], False) -- uint64_t
        ]

{- | Basic numeric (and similar) Haskell and Rust types.

There should be no conversion required here as these should have identical
memory layouts.
-}
basic :: Q Context
basic =
    mkContext
        [ ([ty| char  |], [t|Char|], True) -- 4 bytes
        , ([ty| i8    |], [t|Int8|], True)
        , ([ty| i16   |], [t|Int16|], True)
        , ([ty| i32   |], [t|Int32|], True)
        , ([ty| i64   |], [t|Int64|], True)
        , ([ty| u8    |], [t|Word8|], True)
        , ([ty| u16   |], [t|Word16|], True)
        , ([ty| u32   |], [t|Word32|], True)
        , ([ty| u64   |], [t|Word64|], True)
        , ([ty| f32   |], [t|Float|], True)
        , ([ty| f64   |], [t|Double|], True)
        , ([ty| isize |], [t|Int|], True)
        , ([ty| usize |], [t|Word|], True)
        , ([ty| bool  |], [t|Word8|], True)
        , ([ty| ()    |], [t|()|], True)
        ]

{- | Basic unboxed Haskell types

TODO: MutableByteArray#
-}
ghcUnboxed :: Q Context
ghcUnboxed =
    mkContext
        [ ([ty| char      |], [t|Char#|], False)
        , ([ty| isize     |], [t|Int#|], False)
        , ([ty| usize     |], [t|Word#|], False)
        , ([ty| f32       |], [t|Float#|], False)
        , ([ty| f64       |], [t|Double#|], False)
        , ([ty| *const i8 |], [t|ByteArray#|], False)
        ]

{- | Haskell pointers map onto Rust pointers. Note that unlike Rust, Haskell
doesn't really distinguish between pointers pointing to immutable memory from
those pointing to to mutable memory, so it is up to the user to enforce this.

NOTE: pointers will not support pointed types that require an intermediate
      Rust type.
-}
pointers :: Q Context
pointers = do
    ptrConT <- [t|Ptr|]
    pure (Context ([rule], [rev ptrConT], [constPtr, mutPtr]))
  where
    rule (Ptr _ t _) context
        | First (Just (t', Nothing)) <- lookupRTypeInContext t context = pure ([t|Ptr $t'|], Nothing)
    rule _ _ = mempty

    rev ptrConT (AppT ptrCon t) context
        | ptrCon == ptrConT = do
            t' <- lookupHTypeInContext t context
            pure (Ptr Mutable <$> t' <*> pure ())
    rev _ _ _ = mempty

    constPtr =
        unlines
            [ "impl<T> MarshalInto<*const T> for *const T {"
            , "  fn marshal(self) -> *const T { self }"
            , "}"
            ]

    mutPtr =
        unlines
            [ "impl<T> MarshalInto<*mut T> for *mut T {"
            , "  fn marshal(self) -> *mut T { self }"
            , "}"
            ]

foreignPointers :: Q Context
foreignPointers = do
    foreignPtrT <- [t|ForeignPtr|]
    pure $ Context ([rule], [rev foreignPtrT], [foreignPtr, constPtr, mutPtr])
  where
    rule (Rptr _ _ t _) context
        | First (Just (t', Nothing)) <- lookupRTypeInContext t context = pure ([t|ForeignPtr $t'|], Nothing)
    rule (PathTy Nothing (Path False [PathSegment "ForeignPtr" (Just (AngleBracketed [] [t] [] _)) _] _) _) context
        | First (Just (t', Nothing)) <- lookupRTypeInContext t context = pure ([t|ForeignPtr $t'|], Nothing)
    rule (PathTy Nothing (Path False [PathSegment "Option" (Just (AngleBracketed [] [PathTy Nothing (Path False [PathSegment "ForeignPtr" (Just (AngleBracketed [] [t] [] _)) _] _) _] [] _)) _] _) _) context
        | First (Just (t', Nothing)) <- lookupRTypeInContext t context =
            pure ([t|Maybe (ForeignPtr $t')|], pure . pure $ PathTy Nothing (Path False [PathSegment "ForeignPtr" (Just (AngleBracketed [] [t] [] ())) ()] ()) ())
    rule _ _ = mempty

    rev _ _ _ = mempty

    foreignPtr =
        unlines
            [ "#[repr(C)]"
            , "pub struct ForeignPtr<T>(pub *mut T, pub extern \"C\" fn (*mut T));"
            ]

    constPtr =
        unlines
            [ "impl<'a, T> MarshalInto<&'a T> for &'a T {"
            , "  fn marshal(self) -> &'a T { self }"
            , "}"
            ]

    mutPtr =
        unlines
            [ "impl<T> MarshalInto<*mut T> for ForeignPtr<T> {"
            , "  fn marshal(self) -> *mut T {"
            , "    let ForeignPtr(ptr, _) = self;"
            , "    ptr"
            , "  }"
            , "}"
            , ""
            , "impl<T> MarshalInto<ForeignPtr<T>> for ForeignPtr<T> {"
            , "  fn marshal(self) -> Self {"
            , "    self"
            , "  }"
            , "}"
            , ""
            , "impl<T> From<Box<T>> for ForeignPtr<T> {"
            , "  fn from(p: Box<T>) -> ForeignPtr<T> {"
            , "    extern fn free<T>(ptr: *mut T) {"
            , "      let t = unsafe { Box::from_raw(ptr) };"
            , "      drop(t);"
            , "    }"
            , "    ForeignPtr(std::ptr::from_mut(Box::leak(p)), free)"
            , "  }"
            , "}"
            , ""
            , "impl<'a, T> MarshalInto<&'a mut T> for &'a mut T {"
            , "  fn marshal(self) -> &'a mut T { self }"
            , "}"
            , ""
            , "impl<T> MarshalInto<ForeignPtr<T>> for Option<ForeignPtr<T>> {"
            , "  fn marshal(self) -> ForeignPtr<T> {"
            , "    extern fn panic<T>(_ptr: *mut T) {"
            , "      panic!(\"Attempted to free a null ForeignPtr\")"
            , "    }"
            , "    self.unwrap_or(ForeignPtr(std::ptr::null_mut(), panic))"
            , "  }"
            , "}"
            ]

{- | This maps a Rust function type into the corresponding 'FunPtr' wrapped
Haskell function type.

Note that as a user, you are still responsible for marshalling values of
type 'FunPtr'. The reason for this is simple: the GHC runtime has no way of
automatically detecting when a pointer to a function is no longer present on
the Rust side.

NOTE: function pointers will not support pointed types that require an intermediate
      Rust type.
-}
functions :: Q Context
functions = do
    funPtrT <- [t|FunPtr|]
    ioT <- [t|IO|]
    pure (Context ([rule], [rev funPtrT ioT], [impl]))
  where
    rule (BareFn _ C _ (FnDecl args retTy False _) _) context = do
        args' <-
            for args $ \arg -> do
                argTy <- case arg of
                    Arg _ argTy _ -> pure argTy
                    _ -> mempty
                case lookupRTypeInContext argTy context of
                    First (Just (t', Nothing)) -> pure t'
                    _ -> mempty

        retTy' <-
            case retTy of
                Nothing -> pure [t|IO ()|]
                Just t ->
                    case lookupRTypeInContext t context of
                        First (Just (t', Nothing)) -> pure t'
                        _ -> mempty

        let hFunTy = foldr (\l r -> [t|$l -> $r|]) retTy' args'
        let hFunPtr = [t|FunPtr $hFunTy|]

        pure (hFunPtr, Nothing)
    rule _ _ = mempty

    rev funPtrT ioT (AppT funPtr t) context
        | funPtr == funPtrT = do
            let ts = getApps t
                args = init ts

                ret = last ts
                ret' = case ret of
                    AppT io r | io == ioT -> r
                    r -> r
                ret'' = case ret' of
                    TupleT 0 -> Nothing
                    r -> Just r

            argsRs <- traverse (`lookupHTypeInContext` context) args
            retRs <- traverse (`lookupHTypeInContext` context) ret''

            let argsRs' :: Q [Arg ()]
                argsRs' = map (\a -> Arg Nothing a ()) <$> sequence argsRs
            let decl = FnDecl <$> argsRs' <*> sequence retRs <*> pure False <*> pure ()
            pure (BareFn Normal C [] <$> decl <*> pure ())
    rev _ _ _ _ = mempty

    getApps :: Type -> [Type]
    getApps (AppT e1 e2) = e1 : getApps e2
    getApps e = [e]

    -- TODO: this only goes up to 16
    impl :: String
    impl = unlines (map implN [1 .. 16])
      where
        vars = [l : i | i <- "" : map show [(1 :: Int) ..], l <- ['T' .. 'Z']]

        implN :: Int -> String
        implN n =
            let vs = intercalate "," (take n vars)
                f = "extern \"C\" fn (" ++ vs ++ ") -> R"
             in unlines
                    [ "impl<" ++ vs ++ ",R> MarshalInto<" ++ f ++ "> for (" ++ f ++ ") {"
                    , "  fn marshal(self) -> (" ++ f ++ ") { self }"
                    , "}"
                    ]
