mergeSort :: (Ord a) => [a] -> [a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge sortedLeft sortedRight
    where
        sortedLeft = mergeSort left
        sortedRight = mergeSort right
        left = take half x
        right = drop half x
        half = length(x) `div` 2
        merge x [] = x
        merge [] x = x
        merge (x:xs) (y:ys) = if x < y then x : merge xs (y : ys)  else y : merge (x : xs) ys

