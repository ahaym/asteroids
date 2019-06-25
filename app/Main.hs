{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import SDL
import System.Random

import Runner
import Types

main :: IO ()
main = do
    initializeAll
    g <- getStdGen
    w <- createWindow "Game" defaultWindow {windowInitialSize = V2 1024 768}
    r <- createRenderer w (-1) defaultRenderer
    let gs = GS 0 500 [A 500 0 100] g Z 0
    runStateT (gameLoop r) gs
    return ()
