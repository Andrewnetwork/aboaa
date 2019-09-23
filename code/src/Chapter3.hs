module Chapter3 where
import Text.Printf

integerModulo :: Integral a => a -> a -> a -> a
integerModulo n x y = mod (x+y) n 
-- >>> (integerModulo 6) 7 0
-- 1
-- >>> (integerModulo 6 0) <$> [0..12]
-- [0,1,2,3,4,5,0,1,2,3,4,5,0]

--operationTable :: (a -> a -> a) -> a -> String
operationMatrix :: (t -> t -> a) -> [t] -> [[a]]
operationMatrix op domain = listToMat (length domain) (length domain) [ op x y | x <- domain, y <- domain]

-- TODO: Clean up. 
printOperationMatrix
  :: (Text.Printf.IsChar a1, Text.Printf.PrintfArg t,
      Text.Printf.PrintfArg a2) =>
     (t -> t -> a2) -> [t] -> a1 -> IO ()
printOperationMatrix op domain opSymbol = 
    putStrLn $ printf "%3s " [opSymbol] ++ " | " ++ (domain >>= printf "%4d" ) ++ "\n" ++
        replicate 32 '-' ++ "\n" ++ (zip domain (operationMatrix op domain) >>= 
            (\(d,x) -> printf "%3d " d ++ " | " ++ (x >>= printf "%4d") ++ "\n"))
-- putStrLn $ operationTable (integerModulo 6) [0..5]
-- printOperationMatrix (integerModulo 6) [0..5] '+' 

listToMat 0 _ _ = []
listToMat _ _ [] = [] 
listToMat nRows nCols ls = take nCols ls : listToMat (pred nRows) nCols (drop nCols ls) 