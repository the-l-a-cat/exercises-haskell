module Main where

import Control.Monad 
import Data.ByteString.Lazy

import Network.HTTP.Conduit
import Text.XML.HXT.Core

main :: IO ()
main  = mapM_ Prelude.putStrLn =<< $ parse "https://hackage.haskell.org/packages/top"
        >>= runX . xshow . (>>> multi (hasName "a"))

    where
        unchar8 = Prelude.map (toEnum . fromEnum) . unpack
        parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp



