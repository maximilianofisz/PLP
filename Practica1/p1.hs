-- 1)

max2_c :: Float -> (Float -> Float)
max2_c x y
    | x >= y = x
    | otherwise = y

normaVectorial_c :: Float -> (Float -> Float)
normaVectorial_c x y = sqrt (x^2 + y^2)


_substract :: Float -> Float -> Float
_substract = flip (-)

predecesor :: Float -> Float
predecesor = _substract 1

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

flipraro :: b -> (a -> b -> c) -> a -> c
flipraro = flip flip

-- 2)

curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> f (x, y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f = \(x, y) -> f x y

-- curryN no es posible, deberiamos poder decir: :: (a,b,...,) -> ... con infinitos argumentos

-- 3)

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs)
    | x > rec = x
    | otherwise = rec
    where rec = maximo xs

-- 3)

_foldr :: (a -> b -> b) -> b -> [a] -> b
_foldr _ z [] = z
_foldr f z (x:xs) = f x (foldr f z xs) 

_sum :: Num a => [a] -> a
_sum x = foldr f 0 x
    where f = \x y -> x + y

_elem :: (Foldable t, Eq a) => a -> t a -> Bool
_elem e x = foldr f False x
    where f = \x y -> x == e || y 

plusplus :: [Integer] -> [Integer] -> [Integer]
plusplus x y = foldr (:) y x

_filter :: (a -> Bool) -> [a] -> [a]
_filter f x = foldr combiner [] x
    where combiner = \x y -> if f x == True then x : y else y

_map :: (a -> b) -> [a] -> [b]
_map f x = foldr combiner [] x
    where combiner = \x y -> f x : y 


mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun comparer x = foldr1 combiner x
    where combiner = \x y -> if comparer x y == True then x else y

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl f [] 
    where f = \ac x -> if length ac == 0 then [x]  else ac ++ [((last ac) + x)] 

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr combiner 0
    where combiner = (-)

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 x = foldr (-) 0 (reverse x)

-- 5 )

entrelazar1 :: [a] -> [a] -> [a]
entrelazar1 [] = id
entrelazar1 (x:xs) = \ys -> if null ys
                                    then x : entrelazar1 xs []
                                    else x : head ys : entrelazar1 xs (tail ys)

entrelazar2 :: [a] -> ([a] -> [a])
entrelazar2 = foldr (\x rec -> \ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)) id