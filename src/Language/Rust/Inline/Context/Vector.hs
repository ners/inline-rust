{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Rust.Inline.Context.Vector where
import Language.Rust.Syntax (Ty(PathTy), Path (Path), PathSegment (PathSegment), PathParameters (AngleBracketed))
import Language.Rust.Inline.Context.Prelude (mkGenPathTy)
import Language.Haskell.TH
import Language.Rust.Quote (ty)
import Data.Maybe (fromMaybe)
import Language.Rust.Inline.Context.Marshalable
import qualified Foreign
import Data.Foldable (foldrM)
import qualified Language.Rust.Inline.Context.Marshalable as Marshalable
import Control.Monad (void)
import Language.Rust.Inline.Context (Context(..), lookupRTypeInContext)

vectors :: Q Context
vectors = pure $ Context ([rule], [], [rustList, impl])
    where
        rule (PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [] [t'] [] _)) _] _) _) ctx
            | t' /= void [ty|u8|] = do
                (t'', rInterOpt) <- lookupRTypeInContext t' ctx
                let inter = mkGenPathTy "Vector" . pure <$> fromMaybe (pure t') rInterOpt
                pure ([t| [$t''] |], Just inter)
        rule _ _ = mempty

        rustList = unlines
            [ "#[repr(C)]"
            , "pub struct Vector<T>(*mut T, usize, extern \"C\" fn (*mut T, usize));"
            , "impl<T> Copy for Vector<T> { }"
            , "impl<T> Clone for Vector<T> { fn clone(&self) -> Self { Vector(self.0, self.1, self.2) } }"
            ]

        impl = unlines
            [ "impl<U, T : MarshalInto<U> + Copy> MarshalInto<Vec<U>> for Vector<T> {"
            , "  fn marshal(self) -> Vec<U> {"
            , "    let Vector(ptr, len, _) = self;"
            , "    let inputs = unsafe { std::slice::from_raw_parts(ptr, len) };"
            , "    let mut vec = Vec::with_capacity(len);"
            , "    for i in 0..len {"
            , "      vec.push(inputs[i].marshal());"
            , "    }"
            , "    vec"
            , "  }"
            , "}"
            , ""
            , "impl<U, T: MarshalInto<U>> MarshalInto<Vector<U>> for Vec<T> {"
            , "  fn marshal(self) -> Vector<U> {"
            , "    let mut vec = Vec::with_capacity(self.len());"
            , "    for x in self.into_iter() {"
            , "      vec.push(x.marshal());"
            , "    }"
            , ""
            , "    let slice = Box::leak(vec.into_boxed_slice());"
            , "    let len = slice.len();"
            , ""
            , "    extern fn free<U>(ptr: *mut U, len: usize) {"
            , "      let data = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr, len)) };"
            , "      drop(data);"
            , "    }"
            , "    Vector(slice.as_mut_ptr(), len, free)"
            , "  }"
            , "}"
            ]

instance Marshalable a => Marshalable [a] where
    sizeOfWith _ = Foreign.sizeOf (undefined :: (Foreign.Ptr (), Word, Foreign.FunPtr (Foreign.Ptr () -> Word -> IO ())))
    alignmentWith _ = Foreign.alignment (undefined :: (Foreign.Ptr (), Word, Foreign.FunPtr (Foreign.Ptr () -> Word -> IO ())))
    withLoc as loc k = Foreign.allocaBytesAligned (length as * sizeOfWith (undefined :: a)) (alignmentWith (undefined :: a)) $ \space -> do
        let pokeSlice = Foreign.poke (Foreign.castPtr loc) (Foreign.castPtr space, length as, Foreign.nullFunPtr)
        let pokeAt i a = withLoc a $ space `Foreign.plusPtr` (i * sizeOfWith (undefined :: a))
        foldr ($) (pokeSlice >> k) $ zipWith pokeAt [0..] as

    sizeOfPeek _ = Foreign.sizeOf (undefined :: (Foreign.Ptr (), Word, Foreign.FunPtr (Foreign.Ptr () -> Word -> IO ())))
    alignmentPeek _ = Foreign.alignment (undefined :: (Foreign.Ptr (), Word, Foreign.FunPtr (Foreign.Ptr () -> Word -> IO ())))
    peek p = do
        (ptr, len, finalizer) <- Foreign.peek (Foreign.castPtr p)
        let peekAtOffset :: Word -> IO a
            peekAtOffset offset = Marshalable.peek $ ptr `Foreign.plusPtr` (fromIntegral offset * sizeOfPeek (undefined :: a))
        list <- foldrM (\a b -> liftA2 (:) (peekAtOffset a) (pure b)) [] $ take (fromIntegral len) [0..]
        freeVector finalizer ptr len
        pure list

foreign import ccall safe "dynamic" freeVector :: Foreign.FunPtr (Foreign.Ptr () -> Word -> IO ()) -> Foreign.Ptr () -> Word -> IO ()
