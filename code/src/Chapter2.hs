{-
    Andrew Ribeiro
    September 2019

    This code was written as I was studying "A Book of Abstract Algebra" by Charles Pinter. 
-}
module Chapter2 where 
import Data.Maybe

------  Generating all operations. (pg 20) ---------
cartesianProd :: Applicative f => f a -> f b -> f (a, b)
cartesianProd a b = (,) <$> a <*> b 
-- >>> cartesianProd "ab" "ab"
-- [('a','a'),('a','b'),('b','a'),('b','b')]

permRepeat :: (Monad m, Num b, Enum b) => m a -> b -> m [a]
permRepeat ls n = mapM (const ls) [1..n]
-- >>> permRepeat "ab" 4
-- ["aaaa","aaab","aaba","aabb","abaa","abab","abba","abbb","baaa","baab","baba",
--  "babb","bbaa","bbab","bbba","bbbb"]

opCreator :: (Eq a) => [(a, a)] -> [a] -> (a -> a -> a)
opCreator domain range = \x y -> fromJust $ lookup (x,y) (zip domain range)
-- >>> (opCreator (cartesianProd "ab" "ab") "abba") 'a' 'b' 
-- 'b' 

createAllOps :: (Eq a) => [a] -> [a -> a -> a] 
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
-- Remark: Half of the operators defined on "ab" are commutative. (there are 16 in total)

-- Associative 
isAssociative :: (Eq (f a), Applicative f) => f a -> (a -> a -> a) -> Bool
isAssociative ls fn = (fn <$> ls <*> (uncurry fn <$> domain)) == (fn <$> (uncurry fn <$> domain) <*> ls) 
                      where domain = cartesianProd ls ls 
-- >>> isAssociative "ab" (createAllOps "ab" !! 0)
-- True 

-- ★ How many operators on the "ab" set are associative? 
-- >>> sum $ fromEnum . isAssociative "ab" <$> createAllOps "ab"
-- 8 
-- Remark: Half of the operators defined on "ab" are associative. 
--         this observation does doesn't hold for all sets, i.e., like on "abc."

identityElement :: (Eq a) => [a] -> (a -> a -> a) -> Maybe a
identityElement ls fn | null identityLs = Nothing
                      | otherwise       = Just $ head identityLs 
                      where identityLs = [e | e <- ls, (fn e <$> ls) == ls && (fn <$> ls <*> pure e) == ls]

hasIdentityElement ls fn = case identityElement ls fn of 
                            Just _ -> True 
                            _ -> False 
-- >>> nOp "ab" 0
-- [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'a')]
-- >>> identityElement "ab" (createAllOps "ab" !! 0)
-- Nothing 
-- >>> nOp "ab" 7
-- [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'b'),(('b','b'),'b')]
-- >>> identityElement "ab" (createAllOps "ab" !! 7)
-- Just 'a' 
--
-- Proof of the necessity of a singular identity: 
--     It is inconceivable for an operation to have two identity elements, as e1 * e2 
--     would need to be equal to e1 and e2, by definition, if they are both identities. 
--         

elmInv :: Eq a => (p -> p -> a) -> [p] -> a -> p -> Maybe p
elmInv fn ls e x | null elmInvLs = Nothing
                 | otherwise     = Just $ head elmInvLs
                 where elmInvLs = [ y | y <- ls, fn x y == e && fn y x == e]
-- >>> elmInv (createAllOps "ab" !! 7) "ab" 'a' 'a' 
-- Just 'a'

inverseFunction ls fn = identityElement ls fn >>= go
                        where go e = let invs = mapMaybe (elmInv fn ls e) ls
                                     in if length invs == length ls 
                                        then Just $ \x -> lookup x (zip ls invs)
                                        else Nothing

allElementsHaveInv set operator = case inverseFunction set operator of 
                                    Just _ -> True 
                                    _ -> False 
-- >>> allElementsHaveInv "ab" <$> createAllOps "ab"
-- [False,False,False,False,False,False,True,False,False,True,False,False,False,False,False,False]
-- >>> nOp "ab" 6
-- [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'b'),(('b','b'),'a')]
-- Remark: 'a' is the identity element and each element is its own inverse. 
-- >>> nOp "ab" 9
-- [(('a','a'),'b'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'b')]
-- Remark: operator 9 is a "dual" of operator 6. In this case 'b' is the identity element and 
--         each element again is its own inverse. 

{-
    Problem 2.C: Operations on a Two-Element Set
    Let A be the two-element set A = {a, b}
-}

-- 2.C.1: Write the tables of all 16 operations on A. 
-- >>> nOp "ab" <$> [0..15]
{-
[
    [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'a')],  -- 0
    [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'b')],  -- 1
    [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'b'),(('b','b'),'a')],  -- 2
    [(('a','a'),'a'),(('a','b'),'a'),(('b','a'),'b'),(('b','b'),'b')],  -- 3
    [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'a'),(('b','b'),'a')],  -- 4
    [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'a'),(('b','b'),'b')],  -- 5
    [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'b'),(('b','b'),'a')],  -- 6
    [(('a','a'),'a'),(('a','b'),'b'),(('b','a'),'b'),(('b','b'),'b')],  -- 7
    [(('a','a'),'b'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'a')],  -- 8
    [(('a','a'),'b'),(('a','b'),'a'),(('b','a'),'a'),(('b','b'),'b')],  -- 9
    [(('a','a'),'b'),(('a','b'),'a'),(('b','a'),'b'),(('b','b'),'a')],  -- 10
    [(('a','a'),'b'),(('a','b'),'a'),(('b','a'),'b'),(('b','b'),'b')],  -- 11
    [(('a','a'),'b'),(('a','b'),'b'),(('b','a'),'a'),(('b','b'),'a')],  -- 12
    [(('a','a'),'b'),(('a','b'),'b'),(('b','a'),'a'),(('b','b'),'b')],  -- 13
    [(('a','a'),'b'),(('a','b'),'b'),(('b','a'),'b'),(('b','b'),'a')],  -- 14
    [(('a','a'),'b'),(('a','b'),'b'),(('b','a'),'b'),(('b','b'),'b')]   -- 15
]
-}

-- 2.C.2: Identify which of the operations are commutative. 
-- >>> isCommutative "ab" <$> createAllOps "ab"
-- [True,True,False,False,False,False,True,True,True,True,False,False,False,False,True,True]

-- 2.C.3: Identify which operations are associative. 
-- >>> isAssociative "ab" <$> createAllOps "ab"
-- [True,True,False,True,False,True,True,True,False,True,False,False,False,False,False,True]

-- 2.C.4: Which operations have an identity element? 
-- >>> hasIdentityElement "ab" <$> createAllOps "ab"
-- [False,True,False,False,False,False,True,True,False,True,False,False,False,False,False,False]

-- 2.C.5: For which of the operations does every element have an inverse? 
-- >>> allElementsHaveInv "ab" <$> createAllOps "ab"
-- [False,False,False,False,False,False,True,False,False,True,False,False,False,False,False,False]