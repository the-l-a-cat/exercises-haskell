module Main where

import Control.Monad 
import Control.Arrow
import qualified Data.ByteString.Lazy as L 
import Network.HTTP.Conduit
import Text.XML.HXT.Core
import Data.List

import Classes
import TagsLexer
import qualified TagsParser

main :: IO ()
main = do
    page <- parse url
    printResult =<< (runX $ page >>> walkA tagPathSomewhere >>> putXmlTree "-" //> getText)
        where
        printResult = mapM_ putStrLn

        

parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
    where
    unchar8 = map (toEnum . fromEnum) . L.unpack

-- url = "https://g.co"
url            = "http://vk.com/dev/methods" 
tagString      = "span.dev_methods_list_span"
-- tagString = "html script"
tagPathSomewhere = (TagsParser.parse . TagsLexer.alexScanTokens) tagString


walk tags = foldr (>>>) returnA [ getChildren >>> isElem >>> hasName tag | tag <- tags ]

walkA :: ArrowXml cat0 => [Tag] -> cat0 XmlTree XmlTree
walkA tags = foldr (>>>) returnA [ (getChildren) >>> isElem >>> deep (tagTest tag) | tag <- tags ]
    where
    tagTest tag = hasName (tagName tag) >>> foldr (>>>) returnA
        [ hasAttrValue (attrName attr) (== attrValue attr) | attr <- tagAttrs tag ]
        

