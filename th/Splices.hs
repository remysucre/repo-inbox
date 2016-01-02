module Splices where

import Language.Haskell.TH
import Language.Haskell.Exts
import Language.Haskell.Exts.Parser
import Language.Haskell.Meta.Syntax.Translate

getDef (ValD _ (NormalB (LetE ds _)) _) = ds
getLet (ValD _ (NormalB ls) _) = ls
