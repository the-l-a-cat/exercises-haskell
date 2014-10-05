module Main where

import Control.Monad 
import Control.Arrow
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Conduit
import Text.XML.HXT.Core
import Data.List

main :: IO ()
main = do
    page <- parse url
    links <- runX $ getContentsFromPath pathToLinks <<< page
    counters <- runX $ getContentsFromPath pathToCounters <<< page
    let lines = zip links counters
    print =<< (runX $ page >>> walkA tagPathSomewhere)
    -- printResult [formatReportLine line | line <- lines, "oauth" `isInfixOf` (fst line) ]
        where
        formatReport names numbers = map formatReportLine (zip names numbers)
        formatReportLine (name, number) = name ++ " \t -> \t " ++ number
        printResult = mapM_ putStrLn

        

parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
    where
    unchar8 = map (toEnum . fromEnum) . L.unpack

url            = "https://g.co" 
pathToLinks    = ["html", "body", "div", "div", "table", "tr", "td", "a"]
pathToCounters = ["html", "body", "div", "div", "table", "tr", "td"]

tagPathSomewhere = [ Tag "html" [], Tag "body" [], Tag "div" [ TagAttr "class" "maia-header" ] ]

getContentsFromPath path = walk path >>> getChildren >>> getText


-- transform = foldr (>>>) returnA
--                 [       walk ["html", "body", "div", "div", "table", "tr", "td"]
--                     -- &&& walk ["html", "body", "div", "div", "table", "tr", "td", "a"]
--                     -- <+> walk ["html", "body", "div", "div", "table", "tr", "td", "a"]
--                 , getChildren
--                 , removeWhiteSpace ]

walk tags = foldr (>>>) returnA [ getChildren >>> isElem >>> hasName tag | tag <- tags ]


type TagAttrs = [ TagAttr ]
data TagAttr = TagAttr { attrName :: String, attrValue :: String }
data Tag = Tag { tagName :: String, tagAttrs :: TagAttrs }

walkA :: ArrowXml cat0 => [Tag] -> cat0 XmlTree XmlTree
walkA tags = foldr (>>>) returnA [ getChildren >>> isElem >>> tagTest tag | tag <- tags ]
    where
    tagTest tag = hasName (tagName tag) >>> foldr (>>>) returnA
        [ hasAttrValue (attrName attr) (== attrValue attr) | attr <- tagAttrs tag ]
        

