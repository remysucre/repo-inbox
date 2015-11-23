{-# LANGUAGE BangPatterns #-}

import Debug.Trace
import Data.Aeson.Types
import Data.Aeson
import Data.Scientific
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import Data.Attoparsec.ByteString as P
import System.Environment

data Weird = Weird Int deriving Show

instance FromJSON Weird where
    parseJSON (Number n) = do
        let Just n' = toBoundedInteger n
        return $ Weird $ trace "a" n'

-- parseArray :: Value -> Parser [Weird]
parseArray = withArray "array" $ \arr ->
               mapM parseJSON (V.toList arr)

main = do 
    [fn] <- getArgs
    fc <- B.readFile fn
    let Just rs = parseMaybe parseArray =<< decode' fc :: Maybe [Weird]
    print $ last rs
