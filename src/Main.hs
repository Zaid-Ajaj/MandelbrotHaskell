module Main where

import Control.Monad

data Complex = Complex Double Double
data Point = Point Integer Integer
data Range = Range Double Double

width :: Double
width = 100.0

height :: Double
height = 100.0

xMin :: Double
xMin = -2.0

xMax :: Double
xMax = 2.0

yMin :: Double
yMin = -2.0

yMax :: Double
yMax = 2.0

maxIter :: Int
maxIter = 255


multiply :: Complex -> Complex -> Complex 
multiply (Complex a b) (Complex c d) = Complex (a * c - d * b) (a * d + b * c)

add :: Complex -> Complex  -> Complex 
add (Complex a b) (Complex c d) = Complex (a + c) (b + d) -- just like vectors

magnitude ::  Complex -> Double
magnitude (Complex a b) = sqrt (a * a + b * b) -- vectors again!

rescale :: Double -> Range -> Range -> Double
rescale x (Range a b) (Range c d) = x * abs(d - c) / abs(b - a) + c

pointToComplex :: Point -> Complex
pointToComplex (Point x y) = Complex x' y' 
    where
        x' = rescale (fromIntegral x) (Range 0.0 width) (Range xMin xMax)
        y' = rescale (fromIntegral y) (Range 0.0 height) (Range yMin yMax)

mandelbrot :: Int -> Complex -> Complex 
mandelbrot 0 c = c
mandelbrot n c = add (multiply z z) c
        where z = mandelbrot (n-1) c

-- fsharp style
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

countIterations :: Complex -> Int
countIterations c =
   [1..]
   |> map (\n -> (n, magnitude $ mandelbrot n c))
   |> filter (\(n, res) -> res > 2.0 || n > maxIter)
   |> (fst . head)


points :: [[Point]]
points = [[Point x y | x <- [0..(floor width)]] |  y <- [0..(floor height)]]
    

iterations :: [[Int]]
iterations = map (map (countIterations . pointToComplex)) points  
    

main :: IO()
main = do
    forM_ iterations $ \ns -> do
        forM_ ns $ \n ->
            if n > 100 then putStr "*"
            else putStr " "
        putStrLn ""
    getLine >>= \_ -> return () -- press ENTER and do nothing
