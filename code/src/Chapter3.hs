module Chapter3 where
import Text.Printf
import Chapter2 
import Data.List 
import Data.Maybe

integerModulo :: Integral a => a -> a -> a -> a
integerModulo n x y = mod (x+y) n 
-- >>> (integerModulo 6) 7 0
-- 1
-- >>> (integerModulo 6 0) <$> [0..12]
-- [0,1,2,3,4,5,0,1,2,3,4,5,0]

listToMat :: (Eq t, Num t, Enum t) => t -> Int -> [a] -> [[a]] 
listToMat 0 _ _ = []
listToMat _ _ [] = [] 
listToMat nRows nCols ls = take nCols ls : listToMat (pred nRows) nCols (drop nCols ls) 
-- >>> listToMat 5 5 (cycle [1,2,3])
-- [[1,2,3,1,2],[3,1,2,3,1],[2,3,1,2,3],[1,2,3,1,2],[3,1,2,3,1]]  

operationTable :: (t -> t -> a) -> [t] -> [[a]]
operationTable op domain = 
    listToMat (length domain) (length domain) [ op x y | x <- domain, y <- domain]
-- >>> operationTable (integerModulo 6) [0..5]
-- [[0,1,2,3,4,5],[1,2,3,4,5,0],[2,3,4,5,0,1],[3,4,5,0,1,2],[4,5,0,1,2,3],[5,0,1,2,3,4]]

-- TODO: Clean up. 
printOperationTableN table domain opSymbol = 
    putStrLn $ printf "%3s " [opSymbol] ++ " | " ++ (domain >>= printf "%4d" ) ++ "\n" ++
        replicate (4*length domain+8) '-' ++ "\n" ++ (zip domain table >>= 
            (\(d,x) -> printf "%3d " d ++ " | " ++ (x >>= printf "%4d") ++ "\n"))
-- >>> printOperationTableN (operationTable (integerModulo 6) [0..5]) [0..5] '+' 
{-
  +  |    0   1   2   3   4   5
--------------------------------
  0  |    0   1   2   3   4   5
  1  |    1   2   3   4   5   0
  2  |    2   3   4   5   0   1
  3  |    3   4   5   0   1   2
  4  |    4   5   0   1   2   3
  5  |    5   0   1   2   3   4
-}

-- Aside: We can now use this to create truth tables for boolean operators. 
-- >>> printOperationTableN (operationTable (createAllOps [0,1] !! 6) [0,1]) [0,1] '*'
{- XOR 
  *  |    0   1
----------------
  0  |    0   1
  1  |    1   0
-}
   
dotProduct = (sum .) . zipWith (*)
-- >>> dotProduct [1,3,-5] [4,-2,-1]

matMul m1 m2 = listToMat (length m1) (length m2) [ dotProduct x y | x <- m1, y <- transpose m2]
-- matMul [[1,0],[0,1]] [[0,1],[1,0]]

data NamedValue a = NamedValue{name :: String, value :: a} 

-- Page 28.
matSet = [ NamedValue "I" [[1,0],[0,1]],   NamedValue "A" [[0,1],[1,0]],
           NamedValue "B" [[0,1],[-1,-1]], NamedValue "C" [[-1,-1],[0,1]],
           NamedValue "D" [[-1,-1],[1,0]], NamedValue "K" [[1,0],[-1,-1]]]

instance Eq a => Eq (NamedValue a) where
    -- We ignore names when testing for equality. 
    (NamedValue n1 v1) == (NamedValue n2 v2) = v1 == v2
    (NamedValue n1 v1) /=  (NamedValue n2 v2) = v1 /= v2 

instance (Show a) => Show (NamedValue a) where
    show (NamedValue n v) = n ++ " = " ++ show v

instance Functor NamedValue where
    fmap fn (NamedValue n v) = NamedValue n (fn v)

findName :: Eq a => a -> [NamedValue a] -> Maybe String 
findName _ [] = Nothing
findName needle (NamedValue n v : xs) | needle == v = Just n
                                      | otherwise = findName needle xs
-- >>> findName [[1,0],[-1,-1]] matSet
-- Just "K"

-- TODO: Clean up. 
printOperationTableS table domain opSymbol = 
    putStrLn $ printf "%3s " [opSymbol] ++ " | " ++ (domain >>= printf "%4s" ) ++ "\n" ++
        replicate (4*length domain+8) '-' ++ "\n" ++ (zip domain table >>= 
            (\(d,x) -> printf "%3s " d ++ " | " ++ (x >>= printf "%4s") ++ "\n"))

namedOperationTable op domain = 
    listToMat (length domain) (length domain) 
    [ fromMaybe "∅" (findName (op (value x) (value y)) domain) | x <- domain, y <- domain]
-- >>> namedOperationTable matMul matSet
{-
[["I","A","B","C","D","K"],["A","I","C","B","K","D"],["B","K","D","A","I","C"],
["C","D","K","I","A","B"],["D","C","I","K","B","A"],["K","B","A","D","C","I"]]
-}
-- >>> printOperationTableS (namedOperationTable matMul matSet) (name <$> matSet) '*'
{- The table on Pg. 29
  *  |    I   A   B   C   D   K
--------------------------------
  I  |    I   A   B   C   D   K
  A  |    A   I   C   B   K   D
  B  |    B   K   D   A   I   C
  C  |    C   D   K   I   A   B
  D  |    D   C   I   K   B   A
  K  |    K   B   A   D   C   I
-}