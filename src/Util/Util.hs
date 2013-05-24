module Util where

type Participant = String



-- readInt, read an int
-- @String -> the number
-- @Int -> int value
readInt :: String -> Int
readInt n = read n



-- createList, create a list a participants for test
-- @Int -> number of participants to create
-- @String -> list of participants
createList :: Int -> String
createList n = '[' : createList' n ++ "]"



-- createList', create a list a participants for test
-- @Int -> number of participants to create
-- @String -> list of participants
createList' :: Int -> String
createList' 1 = "\"e1\""
createList' n = ('\"':'e':(show n)++"\",") ++ createList' (n-1)