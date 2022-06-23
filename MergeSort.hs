mergeSort :: (Ord a) => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge (mergeSort (take half x)) (mergeSort (drop half x))
    where
        half = length(x) `div` 2
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge x [] = x
        merge [] x = x
        merge (x:xs) (y:ys) = if x < y then x : merge xs (y : ys)  else y : merge (x : xs) ys

