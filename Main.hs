module Main where

import Network.HTTP
import Text.XML.HXT.Core

main :: IO ()
main = do
    rsp <- Network.HTTP.simpleHTTP (getRequest "http://www.haskell.org/")
    -- res <- fmap (take 1) (Network.HTTP.getResponseBody rsp)
    corpus <- Network.HTTP.getResponseBody rsp
    let xmltree = readString [withParseHTML yes] corpus
    text <- (runX . xshow) xmltree
    mapM_ putStrLn text



