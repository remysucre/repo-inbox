----------------------------------------------------------------------
-- |
-- Module      : Lockfield
-- Maintainer  : AR
-- Stability   : (stable)
-- Portability : (portable)
--
-- > CVS $Date: 2005/11/11 23:24:34 $ 
-- > CVS $Author: aarne $
-- > CVS $Revision: 1.7 $
--
-- Creating and using lock fields in reused resource grammars.
--
-- AR 8\/2\/2005 detached from 'compile/MkResource'
-----------------------------------------------------------------------------

module GF.Grammar.Lockfield (lockRecType, unlockRecord, lockLabel, isLockLabel) where

import qualified Data.ByteString.Char8 as BS

import GF.Infra.Ident
import GF.Grammar.Grammar
import GF.Grammar.Macros

import GF.Data.Operations

lockRecType :: Ident -> Type -> Err Type
lockRecType c t@(RecType rs) = 
  let lab = lockLabel c in
  return $ if elem lab (map fst rs) || elem (showIdent c) ["String","Int"]
    then t --- don't add an extra copy of lock field, nor predef cats
    else RecType (rs ++ [(lockLabel c,  RecType [])])
lockRecType c t = plusRecType t $ RecType [(lockLabel c,  RecType [])]

unlockRecord :: Ident -> Term -> Err Term
unlockRecord c ft = do
  let (xs,t) = termFormCnc ft
  let lock = R [(lockLabel c,  (Just (RecType []),R []))]
  case plusRecord t lock of
    Ok t' -> return $ mkAbs xs t'
    _ -> return $ mkAbs xs (ExtR t lock)

lockLabel :: Ident -> Label
lockLabel c = LIdent $! BS.append lockPrefix (ident2bs c)

isLockLabel :: Label -> Bool
isLockLabel l = case l of
  LIdent c -> BS.isPrefixOf lockPrefix c
  _        -> False


lockPrefix = BS.pack "lock_"
