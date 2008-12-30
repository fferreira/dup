module Dic
 (
   add
 , get
 , keys
 , groupByKey
 ) where

import Data.List (nub)
--type MultiDic = [(a, b)] 

add :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)] 
add elem dic = elem:dic

get :: (Eq a) => a -> [(a, b)] -> [b]
get elem dic = (map snd . filter ((== elem) . fst)) dic 

keys :: (Eq a) => [(a, b)] -> [a]
keys dic = (nub . map fst) dic

groupByKey :: (Eq a) => [(a, b)] -> [[b]]
groupByKey dic = map (\e -> get e dic) (keys dic) 
