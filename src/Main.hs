module Main where

import Control.Monad

data Complex = C Float Float
    deriving Show
    
data Point = Point Int Int
data Range = Range Float Float

multiply :: Complex -> Complex -> Complex
multiply (C a b) (C c d) = C (a * c - d * b) (a * d + b * c)

add  :: Complex -> Complex -> Complex
add (C x1 y1) (C x2 y2) = C (x1+x2) (y1+y2) -- just like vectors

magnitude :: Complex -> Float
magnitude (C a b) = sqrt (a*a + b*b)

rescale :: Int -> Range -> Range -> Float
rescale x (Range a b) (Range c d) = (fromIntegral x) * abs(d - c) / abs(b - a) + c

pointToComplex :: Point -> Int -> Int -> Range -> Range -> Complex
pointToComplex (Point x y) h w rangeX rangeY = C re im
    where
        re = rescale x (Range 0 (fromIntegral w)) rangeX
        im = rescale y (Range 0 (fromIntegral h)) rangeY

mandelbrot :: Int -> Complex -> Complex
mandelbrot 0 c = c
mandelbrot n c = (multiply z z) `add` c
        where z = mandelbrot (n-1) c

-- fsharp style
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

countIterations :: Complex -> Int
countIterations c =
   [1..]
   |> map (\n -> (n, magnitude $ mandelbrot n c))
   |> filter (\(n, res) -> res > 2.0 || n > 255) -- 255 is max iterations
   |> take 1 -- or (fst . head . take 1)
   |> head 
   |> fst
    
   
   
xRange :: Range
xRange = Range (-2.0) 2.0

yRange :: Range
yRange = Range (-2.0) 2.0

width :: Int
width = 120

height :: Int
height = 80
 
xyPairs :: [[(Int,Int)]]
xyPairs = [ [(x, y) | x <- [1..width]] |  y <- [0..height]]

iterations :: [[Int]]
iterations = map (\pairs -> map (\(x,y) -> countIterations (pointToComplex (Point x y) height width xRange yRange)) pairs) xyPairs

main :: IO()
main = do
    forM_ iterations $ \ns -> do
        forM_ ns $ \n ->
            if n > 100 then putStr "*"
            else putStr " "
        putStrLn ""
    getLine >>= \_ -> return () -- press ENTER and do nothing
