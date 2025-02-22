{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Language.Rust.Inline.Context.Marshalable where

import Foreign
    ( Word8,
      Ptr,
      FunPtr,
      ForeignPtr,
      plusPtr,
      newForeignPtr,
      withForeignPtr,
      castPtr,
      sizeOf,
      alignment, Storable, poke )
import qualified Foreign
import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString(PS))
import qualified Data.ByteString.Unsafe as ByteString
import Foreign.C.Types
import Data.Int
import Data.Word
import Language.Rust.Inline.Storable.Tuple ()

-- | A generalisation of `Storable`'s `with` that respects finalizers and lets us avoid copies for types that can not be `Storable`.
class Marshalable a where
    -- | The size of the `Storable` representation of `a`
    sizeOfWith :: a -> Int
    default sizeOfWith :: Storable a => a -> Int
    sizeOfWith = sizeOf

    alignmentWith :: a -> Int
    default alignmentWith :: Storable a => a -> Int
    alignmentWith = alignment

    -- | Holds a reference to its argument while making it available to foreign code.
    -- By default allocates space to store `WithPtrType a` and makes `a` available as `WithPtrType a` there.
    with
        :: a
        -- ^ The data to marshal
        -> (Foreign.Ptr a -> IO b)
        -- ^ The continuation that takes a pointer to the marshaled data
        -> IO b
    with x k = Foreign.allocaBytesAligned (sizeOfWith (undefined :: a)) (alignmentWith (undefined :: a)) $ \loc -> withLoc x loc (k loc)

    -- | Hold a reference to `a` and `poke`s the `WithPtrType a` into a preallocated location.
    -- Call this if you have a specific memory layout requirement, e.g. to marshal `a` as part of a larger data structure.
    withLoc
        :: a
        -- ^ The data to marshal
        -> Ptr a
        -- ^ The location to marshal into (where to put the pointer to the data)
        -> IO b
        -- ^ The action to run while holding the reference to the data
        -> IO b
    default withLoc :: Storable a => a -> Ptr a -> IO b -> IO b
    withLoc p loc k = poke loc p >> k

    sizeOfPeek :: a -> Int
    default sizeOfPeek :: Storable a => a -> Int
    sizeOfPeek = sizeOf

    alignmentPeek :: a -> Int
    default alignmentPeek :: Storable a => a -> Int
    alignmentPeek = alignment

    -- | `peek` the `Storable` representation and convert it to `a`
    peek
        :: Ptr b
        -- ^ The pointer to peek at
        -> IO a
    default peek :: Storable a => Ptr b -> IO a
    peek = Foreign.peek . castPtr

instance Marshalable ByteString where
    sizeOfWith _ = Foreign.sizeOf (undefined :: (Foreign.Ptr Foreign.Word8, Word))
    alignmentWith _ = Foreign.alignment (undefined :: (Foreign.Ptr Foreign.Word8, Word))
    withLoc (PS ptr off len) loc k = Foreign.withForeignPtr ptr $ \ptr' -> do
        Foreign.poke @(Foreign.Ptr Foreign.Word8, Word) (castPtr loc) (ptr' `Foreign.plusPtr` off, fromIntegral len)
        k
    sizeOfPeek _ = Foreign.sizeOf (undefined :: (Foreign.Ptr Foreign.Word8, Word, Foreign.FunPtr (Foreign.Ptr Foreign.Word8 -> Word -> IO ())))
    alignmentPeek _ = Foreign.alignment (undefined :: (Foreign.Ptr Foreign.Word8, Word, Foreign.FunPtr (Foreign.Ptr Foreign.Word8 -> Word -> IO ())))
    peek p = do
        (ptr, len, finalizer) <- Foreign.peek (castPtr p)
        ByteString.unsafePackCStringFinalizer ptr (fromIntegral len) (bytestringFree finalizer ptr len)

instance Marshalable (Foreign.ForeignPtr a) where
    sizeOfWith = const $ sizeOf (undefined :: (Foreign.Ptr a))
    alignmentWith = const $ alignment (undefined :: (Foreign.Ptr a))
    withLoc fp loc k = Foreign.withForeignPtr fp $ \ptr -> do
        Foreign.poke (castPtr loc) ptr
        k
    sizeOfPeek _ = Foreign.sizeOf (undefined :: (Foreign.Ptr a, Foreign.FunPtr (Foreign.Ptr a -> IO ())))
    alignmentPeek _ = Foreign.alignment (undefined :: (Foreign.Ptr a, Foreign.FunPtr (Foreign.Ptr a -> IO ())))
    peek p = do
        (ptr, finalizer) <- Foreign.peek (castPtr p)
        Foreign.newForeignPtr finalizer ptr

foreign import ccall safe "dynamic" bytestringFree :: Foreign.FunPtr (Foreign.Ptr Foreign.Word8 -> Word -> IO ()) -> Foreign.Ptr Foreign.Word8 -> Word -> IO ()

instance Marshalable CChar
instance Marshalable CSChar
instance Marshalable CUChar
instance Marshalable CShort
instance Marshalable CUShort
instance Marshalable CInt
instance Marshalable CUInt
instance Marshalable CLong
instance Marshalable CULong
instance Marshalable CPtrdiff
instance Marshalable CSize
instance Marshalable CWchar
instance Marshalable CLLong
instance Marshalable CULLong
instance Marshalable CBool
instance Marshalable CIntPtr
instance Marshalable CUIntPtr
instance Marshalable CIntMax
instance Marshalable CUIntMax
instance Marshalable CClock
instance Marshalable CTime
instance Marshalable CUSeconds
instance Marshalable CSUSeconds
instance Marshalable CFloat
instance Marshalable CDouble

-- TODO: Should we marshal these? We have them in the libc context ...
-- instance Marshalable CFile
-- instance Marshalable CFpos

instance Marshalable Int8
instance Marshalable Int16
instance Marshalable Int32
instance Marshalable Int64

instance Marshalable Word8
instance Marshalable Word16
instance Marshalable Word32
instance Marshalable Word64

instance Marshalable Char
instance Marshalable Float
instance Marshalable Double
instance Marshalable Int
instance Marshalable Word
instance Marshalable ()
