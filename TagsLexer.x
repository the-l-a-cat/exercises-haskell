{
module TagsLexer where

import Classes
}

%wrapper "basic"

$char = [a-zA-Z0-9_]

tokens :- 
  $white+             ;
  $char [$char]* { \s -> Label s }
  \#                  { \s -> ID      }
  \.                  { \s -> Class_  }
{
-- Each action has type :: String -> Token

}
