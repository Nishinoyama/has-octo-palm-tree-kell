--
-- https://googology.fandom.com/ja/wiki/%E6%80%A5%E5%A2%97%E5%8A%A0%E9%96%A2%E6%95%B0
--
fast :: (Eq a, Num a) => a -> Integer -> Integer

itr :: (a->a) -> a -> Integer -> a

itr _ n 0 = n
itr f n i = f $ itr f n (i-1)


fast 0 n = n + 1
fast 1 n = n * 2
fast 2 n = n * 2 ^ n
fast a n = itr fa n n
    where fa = fast (a-1)
