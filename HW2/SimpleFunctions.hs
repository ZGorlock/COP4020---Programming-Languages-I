module SimpleFunctions where

-- a)
filterFirst :: (a -> Bool) -> [a] -> [a]

filterFirst _ []     = []
filterFirst p (x:xs)
    | p x       = x:(filterFirst p xs)
    | otherwise = xs


-- b)
filterLast :: (a -> Bool) -> [a] -> [a]

filterLast _ []     = []
filterLast p l = reverse (filterFirst p (reverse l))


-- c)
split :: [a] -> ([a],[a])

split []       = ([],[])
split [x]      = ([x],[])
split (x:y:zs) = (x:left, y:right)
    where
        left  = fst (split zs)
        right = snd (split zs)


-- d)
interleave :: ([a],[a]) -> [a]

interleave ([],[])          = []
interleave (f, [])          = f
interleave ([], s)          = s
interleave ((f:fs), (s:ss)) = f:[s] ++ interleave (fs, ss)


-- e)
merge :: (Ord a) => ([a],[a]) -> [a]

merge ([],[])         = []
merge (f, [])         = f
merge ([], s)         = s
merge ((f:fs),(s:ss))
    | f <= s    = f:(merge (fs, (s:ss)))
    | otherwise = s:(merge ((f:fs), ss))


-- f)
mergeSort :: (Ord a) => [a] -> [a]

mergeSort []    = []
mergeSort [f]   = [f]
mergeSort list  = merge ((mergeSort (fst (split list))), (mergeSort (snd (split list))))