mergeSort :: (Ord a) => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = mergeSort left `merge` mergeSort right
    where
        (left, right) = splitAt half x
        half = length x `div` 2
        merge x [] = x
        merge [] x = x
        merge (x:xs) (y:ys) = if x < y then x : merge xs (y : ys)  else y : merge (x : xs) ys

