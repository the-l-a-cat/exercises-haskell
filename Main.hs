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
main = parse url
    >>= (\x -> runX $ x >>> walkA tagPathSomewhere >>> getAttrValue "href")
    >>= return . map ("https://vk.com/"++) . (take 5)
    >>= mapM printAside
    >>= mapM parse
    -- >>= (\x -> runX $ head x >>> putXmlTree "_" >>> walkA_string "div.dev_sett_block" >>> getText )
    >>= \x -> renderArrow [ x !! 2 , putXmlTree "_", walkA_string "div.dev_sett_block" ] getText
    >>= printResult

        where
        printResult = mapM_ putStrLn
        printAside x = putStrLn x >> return x
        -- chainMonad list = foldr (=<<) (head list) (tail list)
        -- chainArrow = foldr (>>>) Control.Arrow.zeroArrow
        renderArrow list renderF = runX $ foldr (>>>) returnA list >>> renderF



parse = liftM (readString [withParseHTML yes, withWarnings no] . unchar8 ) . simpleHttp
    where
    unchar8 = map (toEnum . fromEnum) . L.unpack

-- url = "https://g.co"
url            = "http://vk.com/dev/methods"
tagString      = "a.dev_methods_list_row"
-- tagString = "html script"
tagPathSomewhere = (TagsParser.parse . TagsLexer.alexScanTokens) tagString

walkA_string = walkA . TagsParser.parse . TagsLexer.alexScanTokens


walk tags = foldr (>>>) returnA [ getChildren >>> isElem >>> hasName tag | tag <- tags ]

walkA :: ArrowXml cat0 => [Tag] -> cat0 XmlTree XmlTree
walkA tags = foldr (>>>) returnA [ (getChildren) >>> isElem >>> deep (tagTest tag) | tag <- tags ]
    where
    tagTest tag = hasName (tagName tag) >>> foldr (>>>) returnA
        [ hasAttrValue (attrName attr) (== attrValue attr) | attr <- tagAttrs tag ]


