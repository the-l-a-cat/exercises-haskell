module Main where

import Control.Monad 
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Conduit
import Text.XML.HXT.Core

main :: IO ()
main =  parse target
    >>= runX . transform
    >>= mapM_ putStrLn . take 5

parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
    where
    unchar8 = map (toEnum . fromEnum) . L.unpack

target = "https://hackage.haskell.org/packages/top"

transform x =
          (getChildren >>> removeWhiteSpace >>> getText)
          <<< walk ["html", "body", "div", "div", "table", "tr", "td"] <<< x

walk (tag:tags) = getChildren >>> isElem >>> hasName tag >>> walk tags 
walk [] = arr (\x -> x)
