import Data.Array
main :: IO ()
main = return ()
campoLimpo :: Int -> Int -> Array (Int, Int) Int
campoLimpo x n = array ((1,1),(x,n)) [((i,j),0)|i <- [1..x],j <- [1..n]]
