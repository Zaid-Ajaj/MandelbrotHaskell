module Main where

import Control.Monad (forM_)
import Mandelbrot


main :: IO()
main = do
    forM_ iterations $ \ns -> do
        forM_ ns $ \n ->
            if n > 100 then putStr "*"
            else putStr " "
        putStrLn ""
    getLine >>= \_ -> return () -- press ENTER and do nothing
