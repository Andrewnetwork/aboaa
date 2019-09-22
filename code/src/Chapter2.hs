module Chapter2 where 
import Data.Maybe

------  Generating all operations. (pg 20) ---------

cartesianProd a b = (,) <$> a <*> b 
-- >>> cartesianProd "ab" "ab"
-- [('a','a'),('a','b'),('b','a'),('b','b')]

permRepeat ls n = mapM (const ls) [1..n]
-- >>> permRepeat "ab" 4
-- ["aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb","baaa","baab","baba",
--  "babb","bbaa","bbab","bbba","bbbb"]

opCreator :: (Eq a) => [(a, a)] -> [a] -> (a -> a -> a)
opCreator domain range = \x y -> fromJust $ lookup (x,y) (zip domain range)
-- >>> (opCreator (cartesianProd "ab" "ab") "abba") 'a' 'b' 
-- 'b' 

createAllOps ls = do
    let domain = cartesianProd ls ls
    let domainLength = length domain
    range <- permRepeat ls domainLength
    return (opCreator domain range)
-- >>> length $ createAllOps "ab"
-- 16
-- >>> (\(x,y) -> (createAllOps "ab" !! 0) x y) <$> [('a','a'),('a','b'),('b','a'),('b','b')]
-- "aaaa"
-- >>> (\(x,y) -> (createAllOps "ab" !! 5) x y) <$> [('a','a'),('a','b'),('b','a'),('b','b')]
-- "abab"

applyOp :: (Eq a) => [a] -> Int -> [a]
applyOp ls n = uncurry fn <$> cartesianProd ls ls
               where fn = createAllOps ls !! n
-- >>> applyOp "ab" 5 
-- "abab"
-- >>> applyOp "ab" <$> [0..15]
-- ["aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb",
--  "baaa","baab","baba","babb","bbaa","bbab","bbba","bbbb"]

