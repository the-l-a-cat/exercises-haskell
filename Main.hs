module Main where

import Control.Monad 
import Data.ByteString.Lazy

import Network.HTTP.Conduit
import Text.XML.HXT.Core

main :: IO ()
main =  parse target
    >>= display {- . xshow -} . transform
    where

    parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
        where
        unchar8 = Prelude.map (toEnum . fromEnum) . unpack

    display xmltree = mapM_ Prelude.putStrLn =<< runX xmltree

target = "https://g.co"

transform x = x >>> getChildren >>> isElem >>> hasName "html" >>> getChildren >>> isElem >>> hasName "head" >>> getChildren >>> isElem >>> hasName "title" >>> getChildren >>> removeWhiteSpace >>> getText
