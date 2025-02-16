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

-- type family WithPtrType a where
--     WithPtrType ByteString = Ptr (Ptr Word8, Word)
--     WithPtrType (ForeignPtr a) = Ptr a
--     WithPtrType a = Ptr a

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

-- instance {-# OVERLAPPABLE #-} (WithPtrType a ~ Ptr a, Storable a) => HasWith a where
--     with = Foreign.with

-- type family PeekType a where
--     PeekType ByteString = (Ptr Word8, Word, FunPtr (Ptr Word8 -> Word -> IO ()))
--     PeekType (ForeignPtr a) = (Ptr a, FunPtr (Ptr a -> IO ()))
--     PeekType a = a

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

-- instance {-# OVERLAPPABLE #-} (PeekType a ~ a, Storable a) => HasPeek a where
--     peek = Foreign.peek

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

-- -- | Generate 'Marshalable' instance for a non-recursive simple algebraic data
-- -- type. The instance follows the usual C layout for determining alignment and
-- -- size.
-- --
-- -- Sum types are implemented as tagged unions.
-- --
-- -- >>> mkMarshalable [t| forall a. Marshalable a => Marshalable (Maybe a) |]
-- --
-- -- Remember to have 'ScopedTypeVariables', 'ExplicitForall', and 'EmptyCase'
-- -- enabled when calling this!
-- mkMarshalable :: TypeQ    -- ^ a type representing the desired instance head
--            -> Q [Dec]  -- ^ the instance declaration
-- mkMarshalable tyq = do
--   pseudoInstHead <- tyq
--   
--   -- Extract the context
--   marshalable <- [t| Marshalable |]
--   (ctx, ty') <-
--     case pseudoInstHead of
--       ForallT _ ctx (AppT s ty) | s == marshalable -> pure (ctx, ty)
--       AppT s ty                 | s == marshalable -> pure ([], ty)
--       _ -> fail "mkMarshalable: malformed 'Marshalable' instance head"
-- 
--   -- Get the type constructors name
--   (_,cons') <- getConstructors ty'
-- 
--   -- Produce the instance
--   methods <- processADT [ (nameCon n, tyArgs) | (n,tyArgs) <- cons' ] 
--   dec <- instanceD (pure ctx) (pure (AppT marshalable ty')) (map pure methods)
--   pure [dec]
-- 
-- mkTupleMarshalable :: Int     -- ^ arity of tuple
--                 -> Q [Dec] -- ^ the instance declaration
-- mkTupleMarshalable n = do
--   marshalable <- [t| Marshalable |]
--   tyVars <- sequence (take n [ newName (c : show i)
--                              | i <- [(1 :: Int)..]
--                              , c <- ['a'..'z']
--                              ])
--   let ctx = [ AppT marshalable (VarT tyVar) | tyVar <- tyVars ]
--   let instHead = AppT marshalable (foldl AppT (TupleT n) (map VarT tyVars))
-- 
--   methods <- processADT [ (tupCon, map VarT tyVars) ]
--   let dec = InstanceD Nothing ctx instHead methods
--   pure [dec]
-- 
-- -- * Constructor utilities
-- data Constructor = Constructor
--   { conPat :: [Pat] -> Pat
--   , conExp :: [Exp] -> Exp
--   }
-- 
-- nameCon :: Name -> Constructor
-- nameCon n = Constructor (ConP n []) (foldl AppE (ConE n))
-- 
-- tupCon :: Constructor
-- tupCon = Constructor TupP (TupE . fmap Just)
-- 
-- 
-- -- * Alignment
-- 
-- -- | This is the information you need to carry along as you visit the fields of
-- -- a struct/union.
-- data Alignment = Alignment
--   { decs        :: [Dec]
--   -- ^ declarations for variables relied on by offset and align
--   
--   , offsetSoFar :: Code Q Int
--   -- ^ total bytes occupied so far by fields
--   
--   , alignSoFar  :: Code Q Int
--   -- ^ size (in bytes) of the largest member in the struct
--   }
-- 
-- -- | Combining alignment means concatenating the dependent declarations, and
-- -- take the maximum for offset and alignment.
-- instance Semigroup Alignment where
--   a1 <> a2 = Alignment
--     { decs = decs a1 <> decs a2
--     , offsetSoFar = [|| $$(offsetSoFar a1) `max` $$(offsetSoFar a2) ||]
--     , alignSoFar = [|| $$(alignSoFar a1) `max` $$(alignSoFar a2) ||]
--     }
-- 
-- -- | The 'mconcat' method calls 'maximum'
-- instance Monoid Alignment where
--   mempty = Alignment [] [|| 0 ||] [|| 1 ||]
--   mappend = (<>)
--   mconcat as = Alignment
--     { decs = concatMap decs as
--     , offsetSoFar = [|| maximum $$(liftCode $ listTE <$> mapM (examineCode . offsetSoFar) as) ||]
--     , alignSoFar = [|| maximum $$(liftCode $ listTE <$> mapM (examineCode . alignSoFar) as) ||]
--     }
-- 
-- -- | This is the state we will bundle along while visiting fields.
-- type StructState = StateT Alignment Q
-- 
-- -- | Make a typed list. This function is like 'listE', but for 'TExp'.
-- listTE :: [TExp a] -> TExp [a]
-- listTE = TExp . ListE . map unType
-- 
-- 
-- -- * Peek and poke helper functions 
-- 
-- -- | Produces a 'do' block for peeking a constructor. The generated code has the
-- -- following shape:
-- --
-- -- @
-- --     do f1 <- ... ptr
-- --        f2 <- ... ptr
-- --        ...
-- --        fn <- ... ptr
-- --        return (Con f1 f2 ... fn)
-- -- @
-- --
-- peekCon :: Constructor       -- ^ name of the constructor
--         -> [Exp -> Q Exp]    -- ^ how to peek every field
--         -> Name              -- ^ the base pointer
--         -> Q Exp             -- ^ a 'do' expression for peeking the constructor
-- peekCon con peekFields ptr = do
--   (ns, binds) <- unzip <$> do
--     for peekFields $ \fldCont -> do
--        n <- newName "n"
--        pure (varE n, bindS (varP n) (fldCont (VarE ptr)))
--   let ret = [e| return $(conExp con <$> sequence ns) |]
--   doE (binds ++ [noBindS ret])
-- 
-- -- | Produces a 'do' block for poking a constructor, along with a pattern for
-- -- extracting out the right fields. Given a pattern like @Con f1 f2 ... fn@, the
-- -- generated block has the following shape:
-- --
-- -- @
-- --     do ... ptr f1
-- --        ... ptr f2
-- --        ...
-- --        ... ptr fn
-- -- @
-- pokeCon :: Constructor       -- ^ name of the constructor
--         -> [Exp -> Q Exp]    -- ^ how to poke every field
--         -> Name              -- ^ the base poniter
--         -> Q (Pat, Exp)      -- ^ a pattern to match, an expression for poking
-- pokeCon con pokeFields ptr = do
--   (ns, stmts) <- unzip <$> do
--     for pokeFields $ \fldCont -> do
--         n <- newName "n"
--         pure (varP n, noBindS [e| $(fldCont (VarE ptr)) $(varE n) |])
--   pat <- conPat con <$> sequence ns
--   expr <- if null stmts then [e| pure () |] else doE stmts
--   return (pat, expr)
-- 
-- 
-- -- * Traversing fields (putting everything together)
-- 
-- -- TODO: look at `alignPtr :: Ptr a -> Int -> Ptr a`
-- 
-- -- | Process a field of a given type.
-- processField :: Type -> StructState (Exp -> Q Exp, Exp -> Q Exp)
-- processField ty = do
--   let alignTy, sizeTy :: Code Q Int
--       alignTy  = Code $ TExp <$> [e| alignment (undefined :: $(pure ty)) |]
--       sizeTy   = Code $ TExp <$> [e| sizeOf    (undefined :: $(pure ty)) |]
-- 
--   -- get state at the end of the last field
--   Alignment prevDecs prevOff prevAlign <- get
-- 
--   -- beginning offset
--   beginOffV <- lift $ newName "beginOff"
--   let beginOffE, beginOff :: Code Q Int
--       beginOffE = [|| $$prevOff + mod (negate $$prevOff) $$alignTy ||]
--       beginOff = Code $ TExp <$> varE beginOffV
--   assignBeginOff <- lift [d| $(varP beginOffV) = $(unType <$> examineCode beginOffE) |]
-- 
--   -- offset after this field
--   newOffV <- lift $ newName "afterOff"
--   let newOffE :: Code Q Int
--       newOffE = [|| $$beginOff + $$sizeTy ||]
--   newOff <- lift (TExp <$> varE newOffV)
--   assignNewOff <- lift [d| $(varP newOffV) = $(unType <$> examineCode newOffE) |] 
-- 
--   -- alignment after this field
--   newAlignV <- lift $ newName "algn"
--   let newAlignE :: Code Q Int
--       newAlignE = [|| $$alignTy `max` $$prevAlign ||]
--   newAlign <- lift (TExp <$> varE newAlignV)
--   assignNewAlign <- lift [d| $(varP newAlignV) = $(unType <$> examineCode newAlignE) |]
--   
--   -- update state
--   put (Alignment { decs = concat [ assignBeginOff
--                                  , assignNewOff
--                                  , assignNewAlign
--                                  , prevDecs
--                                  ]
--                  , offsetSoFar = liftCode (pure newOff)
--                  , alignSoFar = liftCode (pure newAlign)
--                  })
-- 
--   -- TODO: consider degenerate sizeof(..) = 0 cases
--   pure ( \addrE -> [e| peek (castPtr $(pure addrE) `plusPtr` $(unType <$> examineCode beginOff)) |]
--        , \addrE -> [e| poke (castPtr $(pure addrE) `plusPtr` $(unType <$> examineCode beginOff)) |]
--        )
-- 
-- 
-- -- | Process an algebraic data type.
-- --
-- -- TODO: think about the zero constructor case...
-- processADT :: [(Constructor, [Type])]  -- ^ constructors and the types of their fields
--            -> Q [Dec]                  -- ^ methods of the 'Marshalable' class
-- 
-- -- The one constructor case is special - we don't need to specify a tag
-- processADT [(con, fields)] = do
--   
--   initAlign <- mempty
--   (peekPokes, Alignment ds off algn)
--     <- runStateT (traverse processField fields) initAlign
--   let ds' = map pure ds
-- 
--   -- sizeOf
--   sizeOf_    <- do
--     Just sizeOfN <- lookupValueName "sizeOf"
--     funD sizeOfN [clause [wildP]
--                          (normalB [e| let c = $(unType <$> examineCode off)
--                                       in c + mod (negate c) $(unType <$> examineCode algn) |])
--                          ds']
-- 
--   -- alignment
--   alignment_ <- do
--     Just alignmentN <- lookupValueName "alignment"
--     funD alignmentN [clause [wildP] (normalB (unType <$> examineCode algn)) ds']
-- 
--   let (peekFields, pokeFields) = unzip peekPokes
--   
--   -- peek
--   peek_ <- do
--     ptr <- newName "ptr"
--     Just peekN <- lookupValueName "peek"
--     funD peekN [clause [varP ptr] (normalB (peekCon con peekFields ptr)) ds']
-- 
--   -- poke
--   poke_ <- do
--     ptr <- newName "ptr"
--     (cPat,body) <- pokeCon con pokeFields ptr
--     Just pokeN <- lookupValueName "poke"
--     funD pokeN [clause [varP ptr, pure cPat] (normalB (pure body)) ds']
-- 
--   pure [sizeOf_, alignment_, peek_, poke_]
-- 
-- processADT cons = do
-- 
--   let discNum = length cons
--   discTy <- snd . head . dropWhile (\(m,_) -> discNum > m + 1) $
--               [ (fromIntegral (maxBound :: Word8),  [t| Word8  |])
--               , (fromIntegral (maxBound :: Word16), [t| Word16 |])
--               , (fromIntegral (maxBound :: Word32), [t| Word32 |])
--               , (fromIntegral (maxBound :: Word64), [t| Word64 |])
--               ]
-- 
--   initAlign <- mempty
--   (conPeekPokess, algns) <- unzip <$> do
--     for cons $ \(con, fields) -> do
--       (peekPokes, algn) <- runStateT (traverse processField fields) initAlign
--       let (peekFields, pokeFields) = unzip peekPokes
--       pure ((con, peekFields, pokeFields), algn)
--   Alignment ds off algn <- mconcat (map pure algns)
--   let discSizeOf = [e| sizeOf (undefined :: $(pure discTy)) |]
--       algn' = [e| $discSizeOf `max` $(unType <$> examineCode algn) |]
--   let ds' = map pure ds
-- 
--   -- sizeOf
--   sizeOf_ <- do
--     Just sizeOfN <- lookupValueName "sizeOf"
--     funD sizeOfN [clause [wildP]
--                          (normalB [e| let c = $(unType <$> examineCode off)
--                                       in $algn' + c + mod (negate c) $algn' |])
--                          ds']
-- 
--   -- alignment
--   alignment_ <- do
--     Just alignmentN <- lookupValueName "alignment"
--     funD alignmentN [clause [wildP] (normalB algn') ds']
-- 
--   -- peek
--   peek_ <- do
--     ptr <- newName "ptr"
--     ptrOff <- newName "ptrOff"
--     d' <- [d| $(varP ptrOff) = $(varE ptr) `plusPtr` $algn' |]
--     disc <- newName "disc"
--     let mtchs = [ match (litP n') (normalB (peekCon con peekFields ptrOff)) []
--                 | (n, (con, peekFields, _)) <- zip [0..] conPeekPokess
--                 , let n' = IntegerL n
--                 ]
--     Just peekN <- lookupValueName "peek"
--     funD peekN
--          [clause [varP ptr]
--                  (normalB (doE [ bindS (varP disc) [e| peek (castPtr $(varE ptr) :: Ptr $(pure discTy)) |]
--                                , noBindS (caseE (varE disc) mtchs)
--                                ]))
--                  (map pure d' ++ ds')]
-- 
--   -- poke
--   poke_ <- do
--     ptr <- newName "ptr"
--     ptrOff <- newName "ptrOff"
--     d' <- [d| $(varP ptrOff) = $(varE ptr) `plusPtr` $algn' |]
--     disc <- newName "disc"
--     let mtchs = [ do { (pat,body) <- patBody
--                      ; match (pure pat)
--                              (normalB (doE (map noBindS [ [e| poke (castPtr $(varE ptr) :: Ptr $(pure discTy)) $(litE n') |]
--                                                           , pure body
--                                                         ])))
--                              []
--                      }
--                 | (n, (con, _, pokeFields)) <- zip [0..] conPeekPokess
--                 , let patBody = pokeCon con pokeFields ptrOff
--                 , let n' = IntegerL n
--                 ]
--     Just pokeN <- lookupValueName "poke"
--     funD pokeN
--          [clause [varP ptr, varP disc] (normalB (caseE (varE disc) mtchs)) (map pure d' ++ ds')]
-- 
--   pure [sizeOf_, alignment_, peek_, poke_]

