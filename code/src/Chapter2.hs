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
    return $ opCreator domain range
-- >>> length $ createAllOps "ab"
-- 16
-- >>> (\(x,y) -> (createAllOps "ab" !! 0) x y) <$> [('a','a'),('a','b'),('b','a'),('b','b')]
-- "aaaa"
-- >>> (\(x,y) -> (createAllOps "ab" !! 5) x y) <$> [('a','a'),('a','b'),('b','a'),('b','b')]
-- "abab"
-- Remark: length (createAllOps ls) = (length ls)^(length ls)^2

nOp :: (Eq a) => [a] -> Int -> [((a,a),a)]
nOp ls n = (\(x,y)-> ((x,y),fn x y)) <$> cartesianProd ls ls
           where fn = createAllOps ls !! n
-- >>> nOp "ab" 5 
-- [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'a'),(('b','b'),'b')]

------  Checking operators for algebraic properties. (pg 21) ---------
-- Commutative 
isCommutative :: (Eq a) => [a] -> (a -> a -> a) -> Bool 
isCommutative ls fn = (uncurry fn <$> domain) == (uncurry (flip fn) <$> domain)
                      where domain = cartesianProd ls ls 
-- >>> nOp "ab" 0
-- [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'a')]
-- >>> isCommutative "ab" (createAllOps "ab" !! 0)
-- True 
-- >>> nOp "ab" 2
-- [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'b'),(('b','b'),'a')]
-- >>> isCommutative "ab" (createAllOps "ab" !! 2)
-- False 

-- ★ How many operators on the "ab" set are commutative? 
-- >>> sum $ fromEnum . isCommutative "ab" <$> createAllOps "ab"
-- 8 
-- Observation: Half of the operators defined on "ab" are commutative. (there are 16 in total)

-- Associative 
isAssociative :: (Eq (f a), Applicative f) => f a -> (a -> a -> a) -> Bool
isAssociative ls fn = (fn <$> ls <*> (uncurry fn <$> domain)) == (fn <$> (uncurry fn <$> domain) <*> ls) 
                      where domain = cartesianProd ls ls 
-- >>> isAssociative "ab" (createAllOps "ab" !! 0)
-- True 

-- ★ How many operators on the "ab" set are associative? 
-- >>> sum $ fromEnum . isAssociative "ab" <$> createAllOps "ab"
-- 8 
-- Observation: Half of the operators defined on "ab" are associative. 
-- this observation does doesn't hold for all sets, i.e., like on "abc."

identityElement ls fn = [e | e <- ls, (fn e <$> ls) == ls && (fn <$> ls <*> pure e) == ls]
-- >>> nOp "ab" 0
-- [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'a')]
-- >>> identityElement "ab" (createAllOps "ab" !! 2)

-- TODO: Make a function which returns the inverse function for an operator. 