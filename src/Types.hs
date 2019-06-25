module Types where

import System.Random

data GState = GS 
    { pVel :: !Int
    , pPos :: !Int
    , as :: ![Asteroid]
    , seed :: !StdGen
    , dir :: !Dir
    , frame :: !Int
    } deriving Show

data Asteroid = A
    { xpos :: !Int
    , ypos :: !Int
    , asize :: !Int
    } deriving Show

data Dir = L | R | B | Z deriving (Eq, Show)
