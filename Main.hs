module Main where

import Control.Monad 
import Control.Arrow
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Conduit
import Text.XML.HXT.Core

main :: IO ()
main = do
    page <- parse url
    links <- runX $ getContentsFromPath pathToLinks <<< page
    counters <- runX $ getContentsFromPath pathToCounters <<< page
    printResult' (formatReport links counters)
        where
        formatReport names numbers = map formatReportLine (zip names numbers)
        formatReportLine (name, number) = name ++ " \t -> \t " ++ number
        printResult = mapM_ putStrLn . take 5
        printResult' = print

        

parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
    where
    unchar8 = map (toEnum . fromEnum) . L.unpack

url            = "https://hackage.haskell.org/packages/top"
pathToLinks    = ["html", "body", "div", "div", "table", "tr", "td", "a"]
pathToCounters = ["html", "body", "div", "div", "table", "tr", "td"]

getContentsFromPath path = walk path >>> getChildren >>> getText


-- transform = foldr (>>>) returnA
--                 [       walk ["html", "body", "div", "div", "table", "tr", "td"]
--                     -- &&& walk ["html", "body", "div", "div", "table", "tr", "td", "a"]
--                     -- <+> walk ["html", "body", "div", "div", "table", "tr", "td", "a"]
--                 , getChildren
--                 , removeWhiteSpace ]

walk tags = foldr (>>>) returnA [ getChildren >>> isElem >>> hasName tag | tag <- tags ]
