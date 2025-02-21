{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Rust.Inline.Context.Marshalable where

import Foreign
    ( Word8,
      Ptr,
      FunPtr,
      ForeignPtr,
      Storable,
      plusPtr,
      newForeignPtr,
      withForeignPtr)
import qualified Foreign
import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString(PS))
import qualified Data.ByteString.Unsafe as ByteString
import Language.Rust.Inline.Context.Prelude ()

class Storable (WithPtrType a) => HasWith a where
    type WithPtrType a
    with :: a -> (Ptr (WithPtrType a) -> IO b) -> IO b
    with x k = Foreign.alloca $ \loc -> withLoc x loc (k loc)

    withLoc :: a -> Ptr (WithPtrType a) -> IO b -> IO b

instance {-# OVERLAPPING #-} HasWith ByteString where
    type WithPtrType ByteString = (Ptr Word8, Word)
    withLoc (PS ptr off len) loc k = withForeignPtr ptr $ \ptr' ->
        Foreign.poke loc (ptr' `plusPtr` off, fromIntegral len) >> k

instance {-# OVERLAPPING #-} Storable a => HasWith (ForeignPtr a) where
    type WithPtrType (ForeignPtr a) = Ptr a
    withLoc fp loc k = withForeignPtr fp $ \ptr ->
        Foreign.poke loc ptr >> k

class HasPeek a where
    type PeekType a
    peek :: Ptr (PeekType a) -> IO a

foreign import ccall safe "dynamic" bytestringFree :: FunPtr (Ptr Word8 -> Word -> IO ()) -> Ptr Word8 -> Word -> IO ()

instance {-# OVERLAPPING #-} HasPeek ByteString where
    type PeekType ByteString = (Ptr Word8, Word, FunPtr (Ptr Word8 -> Word -> IO ()))
    peek ret = do
        (ptr, len, finalizer) <- Foreign.peek ret
        ByteString.unsafePackCStringFinalizer ptr (fromIntegral len) (bytestringFree finalizer ptr len)

instance {-# OVERLAPPING #-} HasPeek (ForeignPtr a) where
    type PeekType (ForeignPtr a) = (Ptr a, FunPtr (Ptr a -> IO ()))
    peek ret = do
        (ptr, finalizer) <- Foreign.peek ret
        newForeignPtr finalizer ptr

class (HasWith a, HasPeek a) => Marshalable a where

instance (Storable (PeekType a), HasPeek a) => HasPeek (Maybe a) where
    type PeekType (Maybe a) = (Word8, PeekType a)
    peek :: Ptr (Word8, PeekType a) -> IO (Maybe a)
    peek ret = do
        d <- Foreign.peek $ Foreign.castPtr @_ @Word8 ret
        case d of
            0 -> pure Nothing
            _ -> Just <$> peek @a (ret `plusPtr` Foreign.alignment @(PeekType a) undefined)

instance HasWith a => HasWith (Maybe a) where
    type WithPtrType (Maybe a) = (Word8, WithPtrType a)
    withLoc Nothing loc k =
        Foreign.poke (Foreign.castPtr @_ @Word8 loc) 0 >> k
    withLoc (Just a) loc k = 
        let align = Foreign.alignment @(WithPtrType a) undefined
         in do Foreign.poke (Foreign.castPtr @_ @Word8 loc) 1
               withLoc a (Foreign.castPtr loc `plusPtr` align) k
