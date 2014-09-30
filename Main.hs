module Main where

import Control.Monad 
import Data.ByteString.Lazy

import Network.HTTP.Conduit
import Text.XML.HXT.Core

main :: IO ()
main  = simpleHttp "https://hackage.haskell.org/packages/top"
    >>= runX . xshow . (>>> multi (hasName "a"))
      . readString [withParseHTML yes, withWarnings no]
      . unchar8
    >>= mapM_ Prelude.putStrLn

    where
        unchar8 = Prelude.map (toEnum . fromEnum) . unpack



