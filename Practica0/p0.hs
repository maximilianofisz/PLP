valorAbsoluto :: Float -> Float
valorAbsoluto x = sqrt (x**2)

bisiesto :: Int -> Bool
bisiesto x = (mod x 4) == 0

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

esPrimo :: Int -> Bool
esPrimo x = divisoresDe x x 0 == 2 

divisoresDe :: Int -> Int -> Int -> Int
divisoresDe x 0 z = z
divisoresDe x y z | mod x y == 0 = divisoresDe x (y-1) (z+1)
                  | otherwise = divisoresDe x (y-1) z


divisoresPrimosDe :: Int -> Int -> Int -> Int
divisoresPrimosDe x 0 z = z
divisoresPrimosDe x y z | mod x y == 0 && esPrimo y = divisoresPrimosDe x (y-1) (z+1)
                  | otherwise = divisoresPrimosDe x (y-1) z


cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos x = divisoresPrimosDe x x 0


inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

aEntero :: Either Int Bool -> Int
aEntero (Right True) = 1
aEntero (Right False) = 0
aEntero (Left x) = x


limpiar :: String -> String -> String
limpiar [] y = y 
limpiar (x:xs) y | elem x y = limpiar xs (eliminar x y)
                  | otherwise = limpiar xs y

eliminar :: Char -> [Char] -> [Char]
eliminar x (y:ys) | x == y = ys
                  | otherwise = y : eliminar x ys

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio x = promediarLista ((sumarElementos x) / 3) x  

sumarElementos :: [Float] -> Float
sumarElementos [] = 0
sumarElementos (x:xs) = x + sumarElementos xs

promediarLista :: Float -> [Float] -> [Float]
promediarLista _ [] = []
promediarLista x (y:ys) = (y - x) : promediarLista x ys

todosIguales :: [Int] -> Bool
todosIguales (x:[]) = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs) 

data AB a = Nil | Bin (AB a) a (AB a) deriving(Show)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin left value right) = Bin (negacionAB left) (not value) (negacionAB right) 

productoAB :: AB Int -> Int
productoAB Nil = 0
productoAB (Bin left value right) = value * (productoAB left) * (productoAB right)