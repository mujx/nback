{-# LANGUAGE TemplateHaskell #-}

module Audio
  ( playSound,
    allSounds,
    SoundFile (..),
  )
where

import Control.Concurrent
import Control.Monad (unless, when)
import qualified Data.ByteString as Bs
import Data.FileEmbed
import qualified Data.Map.Strict as Map
import Sound.ProteaAudio

data SoundFile
  = C
  | H
  | K
  | L
  | Q
  | R
  | S
  | T
  deriving (Bounded, Enum, Eq, Show)

allSounds :: Map.Map Int SoundFile
allSounds =
  Map.fromList $
    zip [0 ..] (enumFrom (toEnum 0) :: [SoundFile])

soundData :: SoundFile -> Bs.ByteString
soundData C = $(embedFile "sounds/letters/c.wav")
soundData H = $(embedFile "sounds/letters/h.wav")
soundData K = $(embedFile "sounds/letters/k.wav")
soundData L = $(embedFile "sounds/letters/l.wav")
soundData Q = $(embedFile "sounds/letters/q.wav")
soundData R = $(embedFile "sounds/letters/r.wav")
soundData S = $(embedFile "sounds/letters/s.wav")
soundData T = $(embedFile "sounds/letters/t.wav")

waitPayback :: IO ()
waitPayback = do
  n <- soundActive
  when (n > 0) $ do
    threadDelay 1000000
    waitPayback

playSound :: SoundFile -> IO ()
playSound f = do
  result <- initAudio 64 44100 1024
  unless result $ fail "failed to initialize the audio system"
  sample <- sampleFromMemoryWav (soundData f) 1
  soundPlay sample 1 1 0 1
  waitPayback
  finishAudio
