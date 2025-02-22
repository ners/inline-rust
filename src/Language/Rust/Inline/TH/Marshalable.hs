{-|
Module      : Language.Rust.Inline.TH.Marshalable
Description : Generate Marshalable instances
Copyright   : (c) Alec Theriault, 2018
License     : BSD-style
Maintainer  : ners <ners@gmx.ch>
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wwarn #-}         -- TODO: GHC bug around "unused pattern binds" in splices
{-# LANGUAGE TypeApplications #-}
                                   -- TODO: GHC feature around setting extensions from within TH
module Language.Rust.Inline.TH.Marshalable (
  mkMarshalable,
  mkTupleMarshalable,
) where

import Language.Rust.Inline.TH.Utilities

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Trans.State ( StateT(..), get, put )
import Control.Monad.Trans.Class ( lift )
import Data.Traversable          ( for )
import Foreign.Ptr               ( alignPtr, plusPtr, castPtr, Ptr )
import Data.Word                 ( Word8, Word16, Word32, Word64 )
import Language.Rust.Inline.Context.Marshalable
import qualified Foreign

-- | Generate 'Marshalable' instance for a non-recursive simple algebraic data
-- type. The instance follows the usual C layout for determining alignment and
-- size.
--
-- Sum types are implemented as tagged unions.
--
-- >>> mkMarshalable [t| forall a. Marshalable a => Marshalable (Maybe a) |]
--
-- Remember to have 'ScopedTypeVariables', 'ExplicitForall', and 'EmptyCase'
-- enabled when calling this!
mkMarshalable :: TypeQ    -- ^ a type representing the desired instance head
              -> Q [Dec]  -- ^ the instance declaration
mkMarshalable tyq = do
  pseudoInstHead <- tyq
  
  -- Extract the context
  marshalable <- [t| Marshalable |]
  (ctx, ty') <-
    case pseudoInstHead of
      ForallT _ ctx (AppT s ty) | s == marshalable -> pure (ctx, ty)
      AppT s ty                 | s == marshalable -> pure ([], ty)
      _ -> fail "mkMarshalable: malformed 'Marshalable' instance head"

  -- Get the type constructors name
  (_,cons') <- getConstructors ty'

  -- Produce the instance
  decs' <- processADT [ (nameCon n, tyArgs) | (n,tyArgs) <- cons' ]
  pure . pure $ InstanceD Nothing ctx (AppT marshalable ty') decs'

mkTupleMarshalable :: Int     -- ^ arity of tuple
                   -> Q [Dec] -- ^ the instance declaration
mkTupleMarshalable n = do
  marshalable <- [t| Marshalable |]
  tyVars <- sequence (take n [ newName (c : show i)
                             | i <- [(1 :: Int)..]
                             , c <- ['a'..'z']
                             ])
  let ctx c = [ AppT c (VarT tyVar) | tyVar <- tyVars ]
  let instHead c = AppT c (foldl AppT (TupleT n) (map VarT tyVars))

  decs' <- processADT [ (tupCon, map VarT tyVars) ]
  pure . pure $ InstanceD Nothing (ctx marshalable) (instHead marshalable) decs'

-- * Constructor utilities
data Constructor = Constructor
  { conPat :: [Pat] -> Pat
  , conExp :: [Exp] -> Exp
  }

nameCon :: Name -> Constructor
nameCon n = Constructor (ConP n []) (foldl AppE (ConE n))

tupCon :: Constructor
tupCon = Constructor TupP (TupE . fmap Just)


-- * Alignment

-- | This is the information you need to carry along as you visit the fields of
-- a struct/union.
data Alignment = Alignment
  { decs        :: [Dec]
  -- ^ declarations for variables relied on by offset and align
  
  , offsetSoFar :: Code Q Int
  -- ^ total bytes occupied so far by fields
  
  , alignSoFar  :: Code Q Int
  -- ^ size (in bytes) of the largest member in the struct
  }

-- | Combining alignment means concatenating the dependent declarations, and
-- take the maximum for offset and alignment.
instance Semigroup Alignment where
  a1 <> a2 = Alignment
    { decs = decs a1 <> decs a2
    , offsetSoFar = [|| $$(offsetSoFar a1) `max` $$(offsetSoFar a2) ||]
    , alignSoFar = [|| $$(alignSoFar a1) `max` $$(alignSoFar a2) ||]
    }

-- | The 'mconcat' method calls 'maximum'
instance Monoid Alignment where
  mempty = Alignment [] [|| 0 ||] [|| 1 ||]
  mappend = (<>)
  mconcat as = Alignment
    { decs = concatMap decs as
    , offsetSoFar = [|| maximum $$(liftCode $ listTE <$> mapM (examineCode . offsetSoFar) as) ||]
    , alignSoFar = [|| maximum $$(liftCode $ listTE <$> mapM (examineCode . alignSoFar) as) ||]
    }

-- | This is the state we will bundle along while visiting fields.
type StructState = StateT Alignment Q

-- | Make a typed list. This function is like 'listE', but for 'TExp'.
listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . map unType

-- * With and Peek helper functions 

-- | TODO: vkleen will write docs
withCon :: Constructor       -- ^ name of the constructor
        -> [Exp -> Q Exp]    -- ^ how to offset to every field
        -> Name              -- ^ the base pointer
        -> Name              -- ^ the name of the continuation parameter
        -> Q (Pat, Exp)      -- ^ an expression for poking the constructor
withCon con fieldOffsets ptr k = do
  (ns, fields) <- unzip <$> do
    for fieldOffsets $ \offset -> do
       n <- newName "n"
       pure (VarP n, [e| withLoc $(varE n) $(offset (VarE ptr)) |])
  let pat = conPat con ns
  f <- foldr (\b e -> [e| $b $e |]) (varE k) fields
  pure (pat, f)

-- | Produces a 'do' block for peeking a constructor. The generated code has the
-- following shape:
--
-- @
--     do f1 <- ... ptr
--        f2 <- ... ptr
--        ...
--        fn <- ... ptr
--        return (Con f1 f2 ... fn)
-- @
--
peekCon :: Constructor       -- ^ name of the constructor
        -> [Exp -> Q Exp]    -- ^ how to offset to every field
        -> Name              -- ^ the base pointer
        -> Q Exp             -- ^ a 'do' expression for peeking the constructor
peekCon con fieldOffsets ptr = do
  (ns, binds) <- unzip <$> do
    for fieldOffsets $ \offset -> do
       n <- newName "n"
       pure (varE n, bindS (varP n) [e| peek $(offset (VarE ptr)) |])
  let ret = [e| return $(conExp con <$> sequence ns) |]
  doE (binds ++ [noBindS ret])

alignQInt :: Q Exp -> Q Exp -> Q Exp
alignQInt size alignment = [e| (($size + $alignment - 1) `div` $alignment) * $alignment |]

alignCodeInt :: Code Q Int -> Code Q Int -> Code Q Int
alignCodeInt size alignment = [|| (($$size + $$alignment - 1) `div` $$alignment) * $$alignment ||]

-- * Traversing fields (putting everything together)

-- | Process a field of a given type.
processField :: Name -> Name -> Type -> StructState (Exp -> Q Exp)
processField alignment sizeOf ty = do
  let alignTy, sizeTy :: Code Q Int
      alignTy  = Code $ TExp <$> [e| $(varE alignment) (undefined :: $(pure ty)) |]
      sizeTy   = Code $ TExp <$> [e| $(varE sizeOf)    (undefined :: $(pure ty)) |]

  -- get state at the end of the last field
  (Alignment prevDecs prevOff prevAlign) <- get

  -- where to peek: align (prevOff) currentAlign
  -- new total alignment: max prevAlign currentAlign
  -- new offset: where to peek + currentSize

  -- beginning offset
  beginOffV <- lift $ newName "beginOff"
  let beginOffE, beginOff :: Code Q Int
      beginOffE = alignCodeInt prevOff alignTy
      beginOff = Code $ TExp <$> varE beginOffV
  assignBeginOff <- lift [d| $(varP beginOffV) = $(unType <$> examineCode beginOffE) |]

  -- offset after this field
  newOffV <- lift $ newName "afterOff"
  let newOffE :: Code Q Int
      newOffE = [|| $$beginOff + $$sizeTy ||]
  newOff <- lift (TExp <$> varE newOffV)
  assignNewOff <- lift [d| $(varP newOffV) = $(unType <$> examineCode newOffE) |] 

  -- alignment after this field
  newAlignV <- lift $ newName "algn"
  let newAlignE :: Code Q Int
      newAlignE = [|| max $$alignTy $$prevAlign ||]
  newAlign <- lift (TExp <$> varE newAlignV)
  assignNewAlign <- lift [d| $(varP newAlignV) = $(unType <$> examineCode newAlignE) |]
  
  -- update state
  put (Alignment { decs = concat [ assignBeginOff
                                 , assignNewOff
                                 , assignNewAlign
                                 , prevDecs
                                 ]
                 , offsetSoFar = liftCode (pure newOff)
                 , alignSoFar = liftCode (pure newAlign)
                 })

  -- TODO: consider degenerate sizeof(..) = 0 cases
  pure $ \addrE -> [e| (castPtr $(pure addrE) `plusPtr` $(unType <$> examineCode beginOff)) |]


-- | Process an algebraic data type.
--
-- TODO: think about the zero constructor case...
processADT :: [(Constructor, [Type])]  -- ^ constructors and the types of their fields
           -> Q [Dec]                  -- ^ marshalable implementations

-- The one constructor case is special - we don't need to specify a tag
processADT [(con, fields)] = do
  initAlign <- mempty
  (offsetsWith, Alignment dsWith sizeWith algnWith) <- runStateT (traverse (processField 'alignmentWith 'sizeOfWith) fields) initAlign
  (offsetsPeek, Alignment dsPeek sizePeek algnPeek) <- runStateT (traverse (processField 'alignmentPeek 'sizeOfPeek) fields) initAlign

  sizeOfWith' <- funD
    (mkName "sizeOfWith")
    [clause [wildP]
        (NormalB . unType <$> examineCode (alignCodeInt sizeWith algnWith))
        (pure <$> dsWith)]

  alignmentWith' <- funD
    (mkName "alignmentWith")
    [clause [wildP]
        (NormalB . unType <$> examineCode algnWith)
        (pure <$> dsWith)]

  withLoc' <- do
    ptr <- newName "ptr"
    k <- newName "k"
    (pat, body) <- withCon con offsetsWith ptr k
    funD (mkName "withLoc") [clause [pure pat, varP ptr, varP k] (normalB $ pure body) (pure <$> dsWith)]

  sizeOfPeek' <- funD
    (mkName "sizeOfPeek")
    [clause [wildP]
        (NormalB . unType <$> examineCode (alignCodeInt sizePeek algnPeek))
        (pure <$> dsPeek)]

  alignmentPeek' <- funD
    (mkName "alignmentPeek")
    [clause [wildP]
        (NormalB . unType <$> examineCode algnPeek)
        (pure <$> dsPeek)]

  peek' <- do
    ptr <- newName "ptr"
    funD (mkName "peek") [clause [varP ptr] (normalB (peekCon con offsetsPeek ptr)) (pure <$> dsPeek)]

  pure [sizeOfWith', alignmentWith', withLoc', sizeOfPeek', alignmentPeek', peek']

processADT cons = do
  let discNum = length cons
  discTy <- snd . head . dropWhile (\(m,_) -> discNum > m + 1) $
              [ (fromIntegral (maxBound :: Word8),  [t| Word8  |])
              , (fromIntegral (maxBound :: Word16), [t| Word16 |])
              , (fromIntegral (maxBound :: Word32), [t| Word32 |])
              , (fromIntegral (maxBound :: Word64), [t| Word64 |])
              ]
 
  initAlign <- mempty
  (conWithsPeeks, algnsWith, algnsPeek) <- unzip3 <$> do
    for cons $ \(con, fields) -> do
      (offsetsWith, algnWith) <- runStateT (traverse (processField 'alignmentWith 'sizeOfWith) fields) initAlign
      (offsetsPeek, algnPeek) <- runStateT (traverse (processField 'alignmentPeek 'sizeOfPeek) fields) initAlign
      pure ((con, offsetsWith, offsetsPeek), algnWith, algnPeek)
  let (Alignment dsWith offWith algnWith) = mconcat algnsWith
  let (Alignment dsPeek offPeek algnPeek) = mconcat algnsPeek
  let discSizeOf = [e| Foreign.sizeOf (undefined :: $(pure discTy)) |]
      discAlign = [e| Foreign.alignment (undefined :: $(pure discTy)) |]

  sizeOfWith' <- funD
    (mkName "sizeOfWith")
    [clause [wildP]
        (normalB [e| $(alignQInt discSizeOf (unType <$> examineCode algnWith)) + $(unType <$> examineCode offWith) |])
        (pure <$> dsWith)]

  alignmentWith' <- funD
    (mkName "alignmentWith")
    [clause [wildP]
        (normalB [e| max $(discAlign) $(unType <$> examineCode algnWith) |])
        (pure <$> dsWith)]

  withLoc' <- do
    ptr <- newName "ptr"
    ptrOff <- newName "ptrOff"
    k <- newName "k"
    d' <- [d| $(varP ptrOff) = ($(varE ptr) `plusPtr` $(discSizeOf)) `alignPtr` $(unType <$> examineCode algnWith) |]
    x <- newName "x"

    let mtchs = [ do (pat, body) <- patBody
                     match (pure pat)
                           (normalB . doE $ noBindS <$> [ [e| Foreign.poke (Foreign.castPtr $(varE ptr) :: Ptr $(pure discTy)) $(litE n') |]
                                                        , pure body
                                                        ])
                           []
                | (n, (con, offsetsWith, _)) <- zip [0..] conWithsPeeks
                , let patBody = withCon con offsetsWith ptrOff k
                , let n' = IntegerL n
                ]

    funD (mkName "withLoc") [clause [varP x, varP ptr, varP k] (normalB $ caseE (varE x) mtchs) (pure <$> d' ++ dsWith)]

  sizeOfPeek' <- funD
    (mkName "sizeOfPeek")
    [clause [wildP]
        (normalB [e| $(alignQInt discSizeOf (unType <$> examineCode algnPeek)) + $(unType <$> examineCode offPeek) |])
        (pure <$> dsPeek)]

  alignmentPeek' <- funD
    (mkName "alignmentPeek")
    [clause [wildP]
        (NormalB . unType <$> examineCode algnPeek)
        (pure <$> dsPeek)]

  peek' <- do
    ptr <- newName "ptr"
    ptrOff <- newName "ptrOff"
    d' <- [d| $(varP ptrOff) = ($(varE ptr) `plusPtr` $(discSizeOf)) `alignPtr` $(unType <$> examineCode algnPeek) |]
    disc <- newName "disc"
    let mtchs = [ match (litP n') (normalB (peekCon con offsetsPeek ptrOff)) []
                | (n, (con, _, offsetsPeek)) <- zip [0..] conWithsPeeks
                , let n' = IntegerL n
                ]
    funD (mkName "peek")
         [clause [varP ptr]
                 (normalB (doE [ bindS (varP disc) [e| Foreign.peek (castPtr $(varE ptr) :: Ptr $(pure discTy)) |]
                               , noBindS (caseE (varE disc) mtchs)
                               ]))
                 (pure <$> d' ++ dsPeek)]
 
  pure [sizeOfWith', alignmentWith', withLoc', sizeOfPeek', alignmentPeek', peek']
