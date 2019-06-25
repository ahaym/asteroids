{-# LANGUAGE RecordWildCards #-}

module Runner where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import Linear
import SDL hiding (get)

import Game
import Types

type GM = StateT GState IO

gameLoop :: Renderer -> GM ()
gameLoop rend = do
    events <- liftIO pollEvents
    mapM_ getDir events

    modify' stepEnv
    drawGame rend
    gs <- get
    present rend
    if check gs then liftIO $ putStrLn "You Lost."
        else delay 18 >> gameLoop rend

getDir :: Event -> GM ()
getDir ev = do
    d <- gets dir
    let dir' = case eventPayload ev of
            KeyboardEvent ke -> case keyboardEventKeyMotion ke of
                Pressed -> fromMaybe d (evDir ke)
                Released -> if evDir ke == Just d then Z else d
            _ -> d
        evDir k = case keysymKeycode (keyboardEventKeysym k) of
            KeycodeA -> Just L
            KeycodeD -> Just R
            KeycodeS -> Just B
            _ -> Nothing

    modify' $ \g -> g {dir = dir'} 

drawGame :: Renderer -> GM ()
drawGame rend = do
    GS{..} <- get
    let pPos' = fromIntegral pPos

    rendererDrawColor rend $= bgc
    clear rend 

    rendererDrawColor rend $= fgc
    fillRects rend $ VS.fromList (mkRect <$> as)
    drawLines rend $ VS.fromList [P (V2 pPos' 680), P (V2 (pPos' - 20) 700), P (V2 (pPos' + 20) 700), P (V2 pPos' 680)]
    where
        fgc = V4 255 255 255 0
        bgc = V4 0 0 0 0

        mkRect A{..} = Rectangle (P (V2 xpos' ypos')) (V2 l l)
            where
                xpos' = fromIntegral xpos
                ypos' = fromIntegral ypos
                l = fromIntegral asize


