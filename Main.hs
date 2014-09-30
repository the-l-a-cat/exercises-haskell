module Main where

import Control.Monad

import Network.HTTP
import Text.XML.HXT.Core

main :: IO ()
main = Network.HTTP.simpleHTTP (getRequest "http://www.haskell.org/")
    >>= Network.HTTP.getResponseBody
    >>= runX . xshow . readString [withParseHTML yes]
    >>= mapM_ putStrLn


