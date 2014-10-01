module Main where

import Control.Monad 
import Control.Arrow
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Conduit
import Text.XML.HXT.Core

main :: IO ()
main =  parse target
    >>= runX . (<<<) transform
    >>= mapM_ putStrLn . take 5

parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
    where
    unchar8 = map (toEnum . fromEnum) . L.unpack

target = "https://hackage.haskell.org/packages/top"

transform = walk ["html", "body", "div", "div", "table", "tr", "td"]
        >>> getChildren
        >>> removeWhiteSpace
        >>> getText

walk tags = foldr (>>>) returnA [ getChildren >>> isElem >>> hasName tag | tag <- tags ]
