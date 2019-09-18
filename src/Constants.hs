module Constants
  ( restDuration,
    stimulusDuration,
    commonMatchesNum,
    modalityMatchesNum,
    matchesNum,
    numTrials,
  )
where

-- | Number of ms between trials.
restDuration :: Int
restDuration = 2500

-- | The duration of the cues in ms.
stimulusDuration :: Int
stimulusDuration = 500

commonMatchesNum :: Int
commonMatchesNum = 2

modalityMatchesNum :: Int
modalityMatchesNum = 4

matchesNum :: Int
matchesNum = commonMatchesNum + modalityMatchesNum

numTrials :: Int -> Int
numTrials lvl = 20 + lvl
