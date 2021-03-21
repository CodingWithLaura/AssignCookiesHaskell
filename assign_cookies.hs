import Data.List

assign_cookies :: [Int] -> [Int] -> Int
assign_cookies xs ys = assign_cookies_sorted (sort xs) (sort ys) 

assign_cookies_sorted :: [Int] -> [Int] -> Int
assign_cookies_sorted [] _  = 0
assign_cookies_sorted _  [] = 0
assign_cookies_sorted (x:xs) (y:ys) | x <= y = 1 + (assign_cookies_sorted xs ys)
                                    | otherwise = (assign_cookies_sorted (x:xs) ys)
