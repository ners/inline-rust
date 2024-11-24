{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Language.Rust.Inline
Description : Quasiquotes for writing Rust code inline in Haskell
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : ners <ners@gmx.ch>
Stability   : experimental
Portability : GHC
-}
module Language.Rust.Inline (
    -- * Overview

    --
    -- $overview
    -- $quasiquoters
    -- $safe
    rust,
    rustIO,

    -- ** Unsafe

    --
    -- $unsafe
    rustUnsafe,
    rustUnsafeIO,

    -- ** Interruptible

    --
    -- $interruptible
    rustInterruptible,
    rustInterruptibleIO,

    -- * Contexts
    Context (..),
    RType,
    HType,

    -- ** Using and defining contexts
    setCrateModule,
    setCrateRoot,
    extendContext,
    singleton,
    mkContext,
    lookupRTypeInContext,
    getRTypeInContext,
    lookupHTypeInContext,
    getHTypeInContext,

    -- ** Built-in contexts
    basic,
    ffi,
    libc,
    ghcUnboxed,
    functions,
    pointers,
    prelude,
    bytestrings,
    foreignPointers,

    -- ** Marshalling
    with,
    alloca,
    free,
    new,
    withFunPtr,
    newFunPtr,
    unFunPtr,
    freeHaskellFunPtr,
    withArrayLen,
    withStorableArrayLen,
    newArray,
    withByteString,
    unsafeLocalState,
    mkStorable,
    mkReprC,

    -- * Top-level Rust items
) where

-- externCrate,

import Language.Rust.Inline.Context
import Language.Rust.Inline.Context.ByteString (bytestrings)
import Language.Rust.Inline.Context.Prelude (prelude)
import Language.Rust.Inline.Internal
import Language.Rust.Inline.Marshal
import Language.Rust.Inline.Parser
import Language.Rust.Inline.Pretty
import Language.Rust.Inline.TH.ReprC (mkReprC)
import Language.Rust.Inline.TH.Storable (mkStorable)

import Language.Haskell.TH (pprParendType)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax

import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (newArray, withArrayLen)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils (new, with)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)

import Control.Monad (void)
import Data.List (intercalate)
import Data.Traversable (for)
import Data.Word (Word8)
import System.Random (randomIO)

import qualified Data.ByteString.Unsafe as ByteString
import Foreign.Storable (Storable (..))

{- $overview

This module provides the facility for dropping in bits of Rust code into your
Haskell project.

** How it works

This works by the magic of Template Haskell. In a nutshell, for every Haskell
source with a Rust quasiquote in it, a Rust source file is generated. Into
this file are added

  - all top-level Rust quasiquotes (contents are added in as-is)

  - functions for all expression-level quasiquotes (function arguments
    correspond to referenced Haskell variables)

On the Haskell side, every expression quasiquote generates an FFI import
to match the generated Rust function and is then replaced with an expression
calling that function (passed in as arguments the Haskell variables the
quasiquote used).

The Rust source file is compiled (by `rustc` if there are no extern crates
or by `cargo` - dependencies are placed in `.inline-rust-quasi` - if there
are). Finally, the resulting static library is passed to GHC through
Template Haskell.
-}

{- $quasiquoters

Rust is (like Haskell) an expression based language, so it is sufficient to
make quasiquoters for expressions only. Note that `{ <stmt>; ... }` is a
valid Rust block expression.

As Rust does not distinguish between pure and impure expressions, it is
entirely up to the user of this library to use the correct quasiquoter.
Quasiquoters with `IO` are meant for impure expressions and the rest are for
pure expressions. Incorrectly annotating an impure expression as pure will
/not/ cause a compile-time error but may break type safety and referential
transparency.
-}

{- $safe

Safe quasiquoters are the most simple ones. When in doubt and not overly in
need of performance, use these.
-}

{- | Safe and pure expression quasiquoter. It is up the user to make sure the
Rust expression they use is pure.

This can also be used in a declaration context to just emit raw Rust code.
You can use this to define Rust items that you can use in any quasiquote in
the module.

@
    rustInc x :: Int32 -> Int32
    rustInc x = [rust| i32 { 1i32 + $(x: i32) } |]
@
-}
rust :: QuasiQuoter
rust = rustQuasiQuoter Safe True True

{- | Safe and impure expression quasiquoter. Like 'rust', this can also be used
to emit top-level Rust items when used in declaration context.

@
    rustHello :: Int32 -> IO ()
    rustHello n = [rustIO| () { println!("Your number: {}", $(n: i32)) } |]
@
-}
rustIO :: QuasiQuoter
rustIO = rustQuasiQuoter Safe False True

{- $unsafe

Unsafe quasiquoters have less overhead than safe ones, but they can have
problems if the Rust expression calls back into the Haskell runtime or
if the Rust expression blocks indefinitely.

This [wiki page](wiki.haskell.org/Foreign_Function_Interface#Unsafe_calls)
and the [Haskell Report](www.haskell.org/definition/haskell2010.pdf) section
on "Import Declarations" detail the caveats of `unsafe`.
-}

{- | Unsafe but pure expression quasiquoter. It is up the user to make sure the
Rust expression they use is pure, doesn't block, and doesn't call
back into the Haskell runtime.

Faster, but use with caution.
-}
rustUnsafe :: QuasiQuoter
rustUnsafe = rustQuasiQuoter Unsafe True False

{- | Unsafe and impure expression quasiquoter. It is up the user to make sure
the Rust expression they use doesn't block and doesn't call back into the
Haskell runtime.

Faster, but use with caution.
-}
rustUnsafeIO :: QuasiQuoter
rustUnsafeIO = rustQuasiQuoter Unsafe False False

{- $interruptible

Interruptible quasiquoters are slightly stronger (and slower) than safe ones:
they additionally try to make the foreign call promptly return when a
'throwTo' is directed at a thread making the call.

The [GHC Docs](downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls)
detail the behaviour of 'interruptible'.
-}

{- | Interrupt and pure expression quasiquoter. It is up the user to make sure the
Rust expression they use is pure.

Slower, but safer around exception-heavy code.
-}
rustInterruptible :: QuasiQuoter
rustInterruptible = rustQuasiQuoter Interruptible True False

{- | Interrupt and impure expression quasiquoter.

Slower, but safer around exception-heavy code.
-}
rustInterruptibleIO :: QuasiQuoter
rustInterruptibleIO = rustQuasiQuoter Interruptible False False

{- | Make an expression/declaration quasiquoter.

For expressions, this packages together the work of parsing the quasiquote
contents, generating Haskell FFI bindings, generating and compiling Rust
source, then linking in the Rust object file.

For declarations (if supported), this emits raw code.
-}
rustQuasiQuoter ::
    -- | safety of FFI
    Safety ->
    -- | purity of FFI
    Bool ->
    -- | support declarations
    Bool ->
    QuasiQuoter
rustQuasiQuoter safety isPure supportDecs =
    QuasiQuoter
        { quoteExp = expQuoter
        , quotePat = err
        , quoteType = err
        , quoteDec = decQuoter
        }
  where
    who
        | supportDecs = "expressions and declarations"
        | otherwise = "expressions"

    err = const $ fail ("(inline-rust): Only " ++ who ++ " can be quasiquoted")

    expQuoter qq = do
        parsed <- parseQQ qq
        processQQ safety isPure parsed

    decQuoter
        | supportDecs = emitCodeBlock
        | otherwise = err

showTy :: Type -> String
showTy = show . pprParendType

{- | This function sums up the packages. What it does:

   1. Map the Rust type annotations in the quasiquote to their Haskell types.

   2. Generate a Haskell FFI signature for passing all the captured Haskell
      variables to Rust.

   3. Generate a Rust function compatible with the Haskell FFI, and emit it
      to a temporary file (the same file is kept for the whole module)

   4. Generate and return a Haskell call to this function, slotting in the
      right Haskell arguments.
-}
processQQ :: Safety -> Bool -> RustQuasiquoteParse -> Q Exp
processQQ safety isPure (QQParse rustRet rustBody rustNamedArgs) = do
    -- Make a name to thread through Haskell/Rust (see Trac #13054)
    q <- runIO randomIO :: Q Int
    qqName' <- newName $ "quasiquote" ++ show (abs q)
    qqName <- newName (show qqName')
    let qqStrName = show qqName

    recordFFIName qqStrName

    -- Find out what the corresponding Haskell representations are for the
    -- argument and return types
    let (rustArgNames, rustArgs) = unzip rustNamedArgs
    (haskRet, reprCRet) <- getRType (void rustRet)
    (haskArgs, reprCArgs) <- unzip <$> traverse (getRType . void) rustArgs

    -- Convert the Haskell return type to a marshallable FFI type
    (returnFfi, haskRet') <- do
        marshalForm <- ghcMarshallable haskRet
        let fptrRet haskRet' = [t|Ptr (Ptr $(pure haskRet'), FunPtr (Ptr $(pure haskRet') -> IO ())) -> IO ()|]
        ret <- case marshalForm of
            BoxedDirect -> [t|IO $(pure haskRet)|]
            BoxedIndirect -> [t|Ptr $(pure haskRet) -> IO ()|]
            UnboxedDirect
                | isPure -> pure haskRet
                | otherwise ->
                    let retTy = showTy haskRet
                     in fail ("Cannot put unlifted type ‘" ++ retTy ++ "’ in IO")
            ByteString -> [t|Ptr (Ptr Word8, Word, FunPtr (Ptr Word8 -> Word -> IO ())) -> IO ()|]
            ForeignPtr
                | AppT _ haskRet' <- haskRet -> fptrRet haskRet'
                | otherwise -> fail ("Cannot marshal " ++ showTy haskRet ++ " using the ForeignPtr calling convention")
            OptionalForeignPtr
                | AppT _ (AppT _ haskRet') <- haskRet -> fptrRet haskRet'
                | otherwise -> fail ("Cannot marshal " ++ showTy haskRet ++ " as an optional ForeignPtr")
        pure (marshalForm, pure ret)

    -- Convert the Haskell arguments to marshallable FFI types
    (marshalForms, haskArgs') <- fmap unzip $
        for haskArgs $ \haskArg -> do
            marshalForm <- ghcMarshallable haskArg
            case marshalForm of
                BoxedIndirect
                    | returnFfi == UnboxedDirect ->
                        let argTy = showTy haskArg
                            retTy = showTy haskRet
                         in fail
                                ( "Cannot pass an argument ‘"
                                    ++ argTy
                                    ++ "’"
                                    ++ " indirectly when returning an unlifted type "
                                    ++ "‘"
                                    ++ retTy
                                    ++ "’"
                                )
                    | otherwise -> do
                        ptr <- [t|Ptr $(pure haskArg)|]
                        pure (BoxedIndirect, ptr)
                ByteString -> do
                    rbsT <- [t|Ptr (Ptr Word8, Word)|]
                    pure (ByteString, rbsT)
                ForeignPtr
                    | AppT _ haskArg' <- haskArg -> do
                        ptr <- [t|Ptr $(pure haskArg')|]
                        pure (ForeignPtr, ptr)
                    | otherwise -> fail ("Cannot marshal " ++ showTy haskRet ++ " using the ForeignPtr calling convention")
                OptionalForeignPtr
                    | AppT _ (AppT _ haskArg') <- haskArg -> do
                        ptr <- [t|Ptr $(pure haskArg')|]
                        pure (OptionalForeignPtr, ptr)
                    | otherwise -> fail ("Cannot marshal " ++ showTy haskRet ++ " as an optional ForeignPtr")
                _ -> pure (marshalForm, haskArg)

    -- Generate the Haskell FFI import declaration and emit it
    bsFree <- newName $ "bsFree" ++ show (abs q)
    bsFreeSig <- [t|FunPtr (Ptr Word8 -> Word -> IO ()) -> Ptr Word8 -> Word -> IO ()|]
    haskSig <- foldr (\l r -> [t|$(pure l) -> $r|]) haskRet' haskArgs'
    let ffiImport = ForeignD (ImportF CCall safety qqStrName qqName haskSig)
    let ffiBsFree = ForeignD (ImportF CCall Safe "dynamic" bsFree bsFreeSig)
    addTopDecls [ffiImport, ffiBsFree]

    -- Generate the Haskell FFI call
    let goArgs ::
            [Q Exp] ->
            -- \^ arguments accumulated so far (reversed)
            [(String, MarshalForm)] ->
            -- \^ remaining arguments to process
            Q Exp
        -- \^ FFI call

        -- Once we run out of arguments we call the quasiquote function with all the
        -- accumulated arguments. If the return value is not marshallable, we have to
        -- 'alloca' some space to put the return value.
        goArgs acc []
            | returnFfi == ByteString = do
                ret <- newName "ret"
                ptr <- newName "ptr"
                len <- newName "len"
                finalizer <- newName "finalizer"
                [e|
                    alloca
                        ( \($(varP ret)) -> do
                            $(appsE (varE qqName : reverse (varE ret : acc)))
                            ($(varP ptr), $(varP len), $(varP finalizer)) <- peek $(varE ret)
                            ByteString.unsafePackCStringFinalizer
                                $(varE ptr)
                                (fromIntegral $(varE len))
                                ($(varE bsFree) $(varE finalizer) $(varE ptr) $(varE len))
                        )
                    |]
            | returnFfi == ForeignPtr = do
                finalizer <- newName "finalizer"
                ptr <- newName "ptr"
                ret <- newName "ret"
                [e|
                    alloca
                        ( \($(varP ret)) -> do
                            $(appsE (varE qqName : reverse (varE ret : acc)))
                            ($(varP ptr), $(varP finalizer)) <- peek $(varE ret)
                            newForeignPtr $(varE finalizer) $(varE ptr)
                        )
                    |]
            | returnFfi == OptionalForeignPtr = do
                finalizer <- newName "finalizer"
                ptr <- newName "ptr"
                ret <- newName "ret"
                [e|
                    alloca
                        ( \($(varP ret)) -> do
                            $(appsE (varE qqName : reverse (varE ret : acc)))
                            ($(varP ptr), $(varP finalizer)) <- peek $(varE ret)
                            if $(varE ptr) == nullPtr
                                then pure Nothing
                                else Just <$> newForeignPtr $(varE finalizer) $(varE ptr)
                        )
                    |]
            | returnByValue returnFfi = appsE (varE qqName : reverse acc)
            | otherwise = do
                ret <- newName "ret"
                [e|
                    alloca
                        ( \($(varP ret)) ->
                            do
                                $(appsE (varE qqName : reverse (varE ret : acc)))
                                peek $(varE ret)
                        )
                    |]

        -- If an argument is by value, we just stack it into the accumulated arguments.
        -- Otherwise, we use 'with' to get a pointer to its stack position.
        goArgs acc ((argStr, marshalForm) : args) = do
            arg <- lookupValueName argStr
            case arg of
                Nothing -> fail ("Could not find Haskell variable ‘" ++ argStr ++ "’")
                Just argName
                    | marshalForm == ByteString -> do
                        ptr <- newName "ptr"
                        len <- newName "len"
                        bsp <- newName "bsp"
                        [e|
                            withByteString
                                $(varE argName)
                                ( \($(varP ptr)) ($(varP len)) ->
                                    with ($(varE ptr), $(varE len)) (\($(varP bsp)) -> $(goArgs (varE bsp : acc) args))
                                )
                            |]
                    | marshalForm == ForeignPtr -> do
                        ptr <- newName "ptr"
                        [e|
                            withForeignPtr $(varE argName) (\($(varP ptr)) -> $(goArgs (varE ptr : acc) args))
                            |]
                    | marshalForm == OptionalForeignPtr -> do
                        ptr <- newName "ptr"
                        fptr <- newName "fptr"
                        [e|
                            case $(varE argName) of
                                Nothing -> let $(varP ptr) = nullPtr in $(goArgs (varE ptr : acc) args)
                                Just $(varP fptr) ->
                                    withForeignPtr $(varE fptr) (\($(varP ptr)) -> $(goArgs (varE ptr : acc) args))
                            |]
                    | passByValue marshalForm -> goArgs (varE argName : acc) args
                    | otherwise -> do
                        x <- newName "x"
                        [e|
                            with
                                $(varE argName)
                                ( \($(varP x)) ->
                                    $(goArgs (varE x : acc) args)
                                )
                            |]

    let haskCall' = goArgs [] (rustArgNames `zip` marshalForms)
        haskCall =
            if isPure && returnFfi /= UnboxedDirect
                then [e|unsafeLocalState $haskCall'|]
                else haskCall'

    -- Generate the Rust function arguments and the converted arguments
    let (rustArgs', rustConvertedArgs) = unzip $ zipWith mergeArgs rustArgs reprCArgs
        (rustRet', rustConvertedRet) = mergeArgs rustRet reprCRet

        -- mergeArgs :: Ty Span -> Maybe RType -> (Ty Span, Ty Span)
        mergeArgs t Nothing = (t, t)
        mergeArgs t (Just tInter) = (fmap (const mempty) tInter, t)

    -- Generate the Rust function.
    let retByVal = returnByValue returnFfi
        (retArg, retTy, ret)
            | retByVal =
                ( []
                , renderType rustRet'
                , "out.marshal()"
                )
            | otherwise =
                ( ["ret_" ++ qqStrName ++ ": *mut " ++ renderType rustRet']
                , "()"
                , "unsafe { ::std::ptr::write(ret_" ++ qqStrName ++ ", out.marshal()) }"
                )
    void . emitCodeBlock . unlines $
        [ "#[no_mangle]"
        , "pub extern \"C\" fn " ++ qqStrName ++ "("
        , "  "
            ++ intercalate
                ", "
                ( [ s ++ ": " ++ marshal (renderType t)
                  | (s, t, v) <- zip3 rustArgNames rustArgs' marshalForms
                  , let marshal x = if passByValue v then x else "*const " ++ x
                  ]
                    ++ retArg
                )
        , ") -> " ++ retTy ++ " {"
        , unlines
            [ "  let " ++ s ++ ": " ++ renderType t ++ " = " ++ marshal s ++ ".marshal();"
            | (s, t, v) <- zip3 rustArgNames rustConvertedArgs marshalForms
            , let marshal x = if passByValue v then x else "unsafe { ::std::ptr::read(" ++ x ++ ") }"
            ]
        , "  let out: " ++ renderType rustConvertedRet ++ " = (|| {" ++ renderTokens rustBody ++ "})();"
        , "  " ++ ret
        , "}"
        ]

    -- Return the Haskell call to the FFI import
    haskCall
