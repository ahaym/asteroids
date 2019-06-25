{-# LANGUAGE RecordWildCards #-}

module Game where

import Control.Monad.State.Strict
import System.Random

import Types

stepEnv :: GState -> GState
stepEnv gs@GS{..} = GS pVel'' pPos' as3 seed' dir (frame + 1)
    where
        pVel' = case dir of
            L -> pVel - 1
            R -> pVel + 1
            B   | pVel == 0 -> 0 
                | pVel > 0 -> pVel - 1 
                | otherwise ->  pVel + 1
            Z -> pVel

        pVel'' = max (-16) (min 16 pVel')

        pPos' =  (pPos + pVel') `mod` 1024

        as' = flip map as $ \a@A{..} -> a {ypos = ypos + 3}

        as'' = filter ((1300>) . ypos) as'

        (nn, s0) = randomR (0, 6) seed

        sizes :: [Int]
        (sizes, s1) = runState
            (replicateM nn $ state (randomR (50, 200))) s0

        (pos, seed') = runState
            (replicateM nn $ state (randomR (0, 1024))) s1

        nb = zipWith (\x l -> A x (-200) l) pos sizes

        as3 = if frame `mod` 60 == 0 then nb ++ as'' else as'' 

check :: GState -> Bool
check GS{..} = any collide as
    where
        collide A{..} = 
            680 - ypos < asize &&
            680 - ypos > 0 &&
            pPos - xpos < asize &&
            pPos - xpos > 0
