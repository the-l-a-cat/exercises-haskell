{
module TagsParser (parse) where

import Classes
}

%tokentype { Token }

%token label  { Label string }
%token id     { ID     }
%token class { Class_ }

%name parse tags

%%

tags  :  tag          { [$1]               }
      |  tags tag     { $1 ++ [$2]              }

tag   :  label attrs  { Tag (show $1) $2          }
      |  label        { Tag (show $1) []             }

attrs :  attr         { [$1]               }
      |  attrs attr   { $2:$1              }

attr  :  id label     { TagAttr "id" (show $2)    }
      |  class label { TagAttr "class" (show $2) }

{
happyError = error "parse error" 
}


