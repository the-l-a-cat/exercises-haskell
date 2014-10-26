module Classes where


data Token = Label String
           | ID
           | Class_
           | Something -- Something that is surely neither class nor ID.  
        deriving (Eq)

instance Show Token where
    show (Label label) = label
    show (ID) = "ID"
    show (Class_) = "Class_"
    show (Something) = "Something"

type TagAttrs = [ TagAttr ]

data TagAttr = TagAttr
                { attrName :: String
                , attrValue :: String
                }
            deriving (Show)

data Tag = Tag
                { tagName :: String
                , tagAttrs :: TagAttrs
                }
            deriving (Show)
