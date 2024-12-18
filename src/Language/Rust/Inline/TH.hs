
module Language.Rust.Inline.TH ( adtCtx, rustTyCtx, mkStorable, mkTupleStorable ) where

import Language.Rust.Inline.TH.Utilities  ( getTyConOpt, getTyCon )
import Language.Rust.Inline.TH.ReprC
import Language.Rust.Inline.TH.Storable ( mkStorable, mkTupleStorable )
import Language.Rust.Inline.Context
import Language.Rust.Inline.Internal
import Language.Rust.Inline.Pretty

import Language.Haskell.TH ( Name, Q, TypeQ, Type(ForallT) )
import Language.Haskell.TH.Lib ( appT, conT )
import Language.Rust.Data.Ident ( Ident ) 
import Language.Rust.Syntax ( Ty(PathTy), Path(..), PathSegment(..), PathParameters(..) )

import Control.Monad ( unless )
import Data.Maybe  ( isJust, fromMaybe )
import Data.Monoid ( First, Any(..) )

adtCtx :: Name         -- ^ name of the 'Storable' Haskell type 
       -> Ident        -- ^ name of the Rust type
       -> Maybe Ident  -- ^ name of the intermediate Rust type (if there is one)
       -> Int          -- ^ how many generic parameters
       -> [String]     -- ^ impl's of @MarshalInto@
       -> Q Context
adtCtx hADT rEnum rReprCOpt n impls = pure (Context ([ goRType ], [ goHType ], impls))
  where
  goRType :: RType -> Context -> First (Q HType, Maybe (Q RType))
  goRType (PathTy Nothing (Path False [ PathSegment rName params _ ] _) _) ctx = do
    rGen <-
      case params of
        Nothing -> pure []
        Just (AngleBracketed [] tys [] _) -> pure tys
        _ -> mempty

    -- Filter out incorrect path types
    unless (rName == rEnum) mempty
    unless (length rGen == n) mempty

    -- Look up generic args recursively
    (hGen, rInterGenOpt) <- unzip <$> traverse (`lookupRTypeInContext` ctx) rGen

    -- Compute the intermediate #[repr(C)] rust type (if we even need one)
    let needInter = getAny $ foldMap Any (isJust rReprCOpt : map isJust rInterGenOpt)
    let rInter :: Maybe (Q RType)
        rInter = if needInter
                   then let rReprC = fromMaybe rEnum rReprCOpt
                            rInterGen = zipWith (fromMaybe . pure) rGen rInterGenOpt
                        in Just (mkGenPathTy rReprC <$> sequence rInterGen)
                   else Nothing
    
    -- Compute the Haskell type
    let hTy = foldl appT (conT hADT) hGen

    pure (hTy, rInter)
  goRType _ _ = mempty

  goHType :: HType -> Context -> First (Q RType)
  goHType hTy ctx
    | Just (hName, hArgs) <- getTyConOpt hTy = do

        -- Filter out incorrect type constructors
        unless (hName == hADT) mempty
        unless (length hArgs == n) mempty
   
        -- Look up parameters recursively
        rGen <- traverse (`lookupHTypeInContext` ctx) hArgs

        -- Compute the Rust type
        pure (mkGenPathTy rEnum <$> sequence rGen)
  goHType _ _ = mempty

-- | TODO: be flexible around naming of the generated type
rustTyCtx :: TypeQ      -- ^ a Haskell type representing the desired Rust type
          -> Q Context  -- ^ the context for passing from the argument Haskell type into the
                        -- generated Rust one (and back)
rustTyCtx tyq = do

  ty' <- tyq
  
  -- TODO: this work is done again in mkReprC - do it only once and here
  -- Extract the context
  (_, ty) <-
    case ty' of
      ForallT tyvars [] t -> pure (tyvars, t)
      ForallT _      _  _ -> fail "rustTyCtx: type cannot have context"
      t                   -> pure ([], t)

  -- Get the type and its name
  (hADT, args) <- getTyCon ty

  -- Get the current context
  ctx <- getContext 

  -- Generate and emit the Rust types
  (rEnum, rReprCOpt, items, impls) <- mkReprC ctx ty'

  -- Produce the context
  adtCtx hADT rEnum rReprCOpt (length args) (map renderItem (impls ++ items))
  


