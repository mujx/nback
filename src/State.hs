{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module State
  ( Game (..),
    Answer (..),
    Screen (..),
    StatsLine (..),
    initReport,
    readAllStats,
    calcPerf,
    chooseItems,
    generateSeqs,
    getMistakes,
    findAnswers,
    findMatches,
    findMistakes,
    fixMatches,
    decideNextLevel,
    createGame,
    MistakeReport (..),
  )
where

import Audio (SoundFile, allSounds)
import Constants
import Control.Monad (replicateM)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as LBs
import qualified Data.Char as C
import Data.Either (either)
import Data.List ((\\), intersect, sort)
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import System.Directory (doesPathExist)
import qualified System.IO.Strict as S
import System.Random (newStdGen, randomRIO)
import System.Random.Shuffle (shuffle')
import Control.Concurrent.STM (TVar)

data Answer
  = AuditoryMatch
  | VisualMatch
  | BothMatches
  deriving (Eq, Show)

instance Semigroup Answer where
  AuditoryMatch <> AuditoryMatch = AuditoryMatch
  VisualMatch <> VisualMatch = VisualMatch
  AuditoryMatch <> _ = BothMatches
  VisualMatch <> _ = BothMatches
  _ <> _ = BothMatches

data StatsLine
  = StatsLine
      { statsTs :: POSIXTime,
        statsLevel :: Int,
        statsReport :: MistakeReport
      }
  deriving (Show, Generic)

noPrefix :: String -> String
noPrefix = firstToLower . dropWhile C.isLower
  where
    firstToLower [] = []
    firstToLower (x : xs) = C.toLower x : xs

noPrefixOptions :: Options
noPrefixOptions = defaultOptions {fieldLabelModifier = noPrefix}

instance ToJSON StatsLine where
  toJSON = genericToJSON noPrefixOptions

instance FromJSON StatsLine where
  parseJSON = genericParseJSON noPrefixOptions

data MistakeReport
  = MistakeReport
      { _vTP :: Float,
        _vTN :: Float,
        _vFP :: Float,
        _vFN :: Float,
        _aTP :: Float,
        _aTN :: Float,
        _aFP :: Float,
        _aFN :: Float
      }
  deriving (Eq, Show)

initReport :: MistakeReport
initReport = MistakeReport
  { _vTP = -1,
    _vTN = -1,
    _vFP = -1,
    _vFN = -1,
    _aTP = -1,
    _aTN = -1,
    _aFP = -1,
    _aFN = -1
  }

audioTP, audioTN, audioFP, audioFN :: MistakeReport
audioTP = MistakeReport {_vTP = 0, _vTN = 0, _vFP = 0, _vFN = 0, _aTP = 1, _aTN = 0, _aFP = 0, _aFN = 0}
audioTN = MistakeReport {_vTP = 0, _vTN = 0, _vFP = 0, _vFN = 0, _aTP = 0, _aTN = 1, _aFP = 0, _aFN = 0}
audioFP = MistakeReport {_vTP = 0, _vTN = 0, _vFP = 0, _vFN = 0, _aTP = 0, _aTN = 0, _aFP = 1, _aFN = 0}
audioFN = MistakeReport {_vTP = 0, _vTN = 0, _vFP = 0, _vFN = 0, _aTP = 0, _aTN = 0, _aFP = 0, _aFN = 1}

visualTP, visualTN, visualFP, visualFN :: MistakeReport
visualTP = MistakeReport {_vTP = 1, _vTN = 0, _vFP = 0, _vFN = 0, _aTP = 0, _aTN = 0, _aFP = 0, _aFN = 0}
visualTN = MistakeReport {_vTP = 0, _vTN = 1, _vFP = 0, _vFN = 0, _aTP = 0, _aTN = 0, _aFP = 0, _aFN = 0}
visualFP = MistakeReport {_vTP = 0, _vTN = 0, _vFP = 1, _vFN = 0, _aTP = 0, _aTN = 0, _aFP = 0, _aFN = 0}
visualFN = MistakeReport {_vTP = 0, _vTN = 0, _vFP = 0, _vFN = 1, _aTP = 0, _aTN = 0, _aFP = 0, _aFN = 0}

calcPerf :: MistakeReport -> (Float, Float)
calcPerf report = (audioPerf, visualPerf)
  where
    audioPerf = ((_aTP report + _aTN report) * 100) / (_aTP report + _aTN report + _aFP report + _aFN report)
    visualPerf = ((_vTP report + _vTN report) * 100) / (_vTP report + _vTN report + _vFP report + _vFN report)

decideNextLevel :: Int -> MistakeReport -> Int
decideNextLevel lvl report
  | aPerf >= upLim && vPerf >= upLim = lvl + 1
  | aPerf <= lowLim || vPerf <= lowLim = max (lvl - 1) 2
  | otherwise = lvl
  where
    upLim = 90
    lowLim = 70
    (aPerf, vPerf) = calcPerf report

deriveJSON defaultOptions ''MistakeReport

instance Semigroup MistakeReport where
  a <> b = MistakeReport
    { --
      -- Visual
      --
      _vTP = _vTP a + _vTP b,
      _vTN = _vTN a + _vTN b,
      _vFP = _vFP a + _vFP b,
      _vFN = _vFN a + _vFN b,
      --
      -- Audio
      --
      _aTP = _aTP a + _aTP b,
      _aTN = _aTN a + _aTN b,
      _aFP = _aFP a + _aFP b,
      _aFN = _aFN a + _aFN b
    }

instance Monoid MistakeReport where
  mempty = MistakeReport
    { _vTP = 0,
      _vTN = 0,
      _vFP = 0,
      _vFN = 0,
      _aTP = 0,
      _aTN = 0,
      _aFP = 0,
      _aFN = 0
    }

data Screen
  = GameScreen
  | MenuScreen
  | ScoreScreen
  | ProgressScreen
  | TrialEndScreen
  deriving (Eq, Show)

data Game
  = Game
      { -- | The visual sequence chosen for the current trial.
        _visuals :: Map.Map Int Int,
        -- | The sound sequence chosen for the current trial.
        _auditory :: Map.Map Int Int,
        -- | The answers given through out the trial.
        _answers :: Map.Map Int (Maybe Answer),
        -- | Current block in a trial.
        _block :: Int,
        -- | The value of N in n-back.
        _level :: Int,
        -- | The answer given by the user. Nothing if there was no input.
        _answer :: Maybe Answer,
        -- | The UI that will be shown.
        _screen :: Screen,
        -- | Whether the current stimulus must be visible.
        _isActive :: Bool,
        -- | Whether the trial has ended.
        _end :: Bool,
        -- | Available sounds to be played.
        _sounds :: Map.Map Int SoundFile,
        -- | The report of the previous trial
        _lastReport :: MistakeReport,
        -- | A file to save the results after each trial.
        _statsFile :: FilePath,
        -- | Statistics loaded.
        _stats :: [StatsLine],
        -- | Trials per session.
        _trials :: Int,
        -- | Indicate whiter the play/stop signal should be emitted.
        _playing :: TVar Bool,
        -- | Whether the `Play` step was executed succefully.
        -- This guards against the presence of an invalid `Stop` state without a `Play` event.
        _playedSound :: Bool
      }

makeLenses ''Game

compareAnswers ::
  Maybe Answer ->
  -- | ^ The correct answer.
  Maybe Answer ->
  -- | ^ The user provided answer.
  MistakeReport
compareAnswers correct guess = case (correct, guess) of
  --
  -- Audio
  --
  (Just AuditoryMatch, Just VisualMatch) -> audioFN <> visualFP
  (Just AuditoryMatch, Just AuditoryMatch) -> audioTP <> visualTN
  (Just AuditoryMatch, Just BothMatches) -> audioTP <> visualFP
  (Just AuditoryMatch, Nothing) -> audioFN <> visualTN
  --
  -- Visual
  --
  (Just VisualMatch, Just VisualMatch) -> audioTN <> visualTP
  (Just VisualMatch, Just AuditoryMatch) -> audioFP <> visualTN
  (Just VisualMatch, Just BothMatches) -> audioFP <> visualTP
  (Just VisualMatch, Nothing) -> audioTN <> visualFN
  --
  -- Audio & Visual
  --
  (Just BothMatches, Just VisualMatch) -> audioFN <> visualTP
  (Just BothMatches, Just AuditoryMatch) -> audioTP <> visualFN
  (Just BothMatches, Just BothMatches) -> audioTP <> visualTP
  (Just BothMatches, Nothing) -> audioFN <> visualFN
  --
  -- Nothing
  --
  (Nothing, Just VisualMatch) -> audioTN <> visualFP
  (Nothing, Just AuditoryMatch) -> audioFP <> visualTN
  (Nothing, Just BothMatches) -> audioFP <> visualFP
  (Nothing, Nothing) -> audioTN <> visualTN

getMistakes :: Game -> MistakeReport
getMistakes g = findMistakes correctAnswers guesses
  where
    correctAnswers = findAnswers (g ^. level) (g ^. visuals) (g ^. auditory)
    guesses = Map.elems $ g ^. answers

findMistakes :: [Maybe Answer] -> [Maybe Answer] -> MistakeReport
findMistakes ans guess = mconcat $ zipWith compareAnswers ans guess

findAnswers ::
  -- | The current level.
  Int ->
  -- | Hash map with the visual entries.
  Map.Map Int Int ->
  -- | Hash map with the auditory entries.
  Map.Map Int Int ->
  [Maybe Answer]
findAnswers lvl visualSeries auditorySeries =
  zipWith
    (curry
      ( \((i, a), (_, b)) ->
          if i >= lvl
            then
              if a == (visualSeries Map.! (i - lvl)) && b == (auditorySeries Map.! (i - lvl))
                then Just BothMatches
                else
                  if a == (visualSeries Map.! (i - lvl))
                    then Just VisualMatch
                    else
                      if b == (auditorySeries Map.! (i - lvl))
                        then Just AuditoryMatch
                        else Nothing
            else Nothing))
    (Map.toList visualSeries)
    (Map.toList auditorySeries)

randomList ::
  -- | The number of items in the list.
  Int ->
  -- | The upper range limit for each item.
  Int ->
  IO [Int]
randomList n maxIdx = replicateM n $ randomRIO (0, maxIdx)

chooseItems ::
  -- | The total number of items to pick.
  Int ->
  -- | The list to pick the items.
  [a] ->
  -- | The list with the picked items.
  IO [a]
chooseItems num lst =
  take num . shuffle' lst (length lst) <$> newStdGen

-- | Generate a valid random sequence for the nback trial.
generateSeqs ::
  -- | The level we are in.
  Int ->
  -- | The maps with the visual & auditory sequences.
  IO (Map.Map Int Int, Map.Map Int Int)
generateSeqs lvl = do
  let seqSize = numTrials lvl
      maxIndex = 7
      allIndices = [lvl .. seqSize - 1]
  initVisual <- randomList seqSize maxIndex
  initAuditory <- randomList seqSize maxIndex
  visualIndices <- chooseItems modalityMatchesNum allIndices
  auditoryIndices <- chooseItems modalityMatchesNum (allIndices \\ visualIndices)
  commonIndices <- chooseItems commonMatchesNum (allIndices \\ (auditoryIndices ++ visualIndices))
  let visuals' =
        fixMatches
          (Map.fromList $ zip [0 ..] initVisual)
          lvl
          (visualIndices ++ commonIndices)
      auditory' =
        fixMatches
          (Map.fromList $ zip [0 ..] initAuditory)
          lvl
          (auditoryIndices ++ commonIndices)
      auditoryMatches = findMatches lvl auditory'
      visualMatches = findMatches lvl visuals'
      commonMatches = auditoryMatches `intersect` visualMatches
  if length auditoryMatches == matchesNum && length visualMatches == matchesNum && length commonMatches == commonMatchesNum
    then pure (visuals', auditory')
    else generateSeqs lvl

-- | Adjust the series to contain the specified number of matches.
fixMatches ::
  -- | The main series to be changed.
  Map.Map Int Int ->
  -- | The level for which we are generating.
  Int ->
  -- | The indices of the series that need adjustment.
  [Int] ->
  Map.Map Int Int
--- ^ The changed series.
fixMatches series lvl indices =
  Map.mapWithKey
    ( \i v ->
        if i `elem` indices'
          then series Map.! (i - lvl)
          else v
    )
    series
  where
    indices' = sort indices

-- | Find the indexes that are a match.
findMatches ::
  -- | The current level.
  Int ->
  -- | The stimulus series.
  Map.Map Int Int ->
  -- | The matches for the given series & level.
  [Int]
findMatches lvl series =
  map fst $
    filter
      (\(i, x) -> i >= lvl && series Map.! (i - lvl) == x)
      (Map.toList series)

-- | Generate the initial answers map with no responses.
initAnswers :: Int -> Map.Map Int (Maybe Answer)
initAnswers lvl = Map.fromList (zip [0 ..] (replicate (numTrials lvl) Nothing))

readAllStats :: FilePath -> IO [StatsLine]
readAllStats f = do
  checkStatsFile f
  content <- S.readFile f
  pure
    $ map (either error id . eitherDecode . LBs.pack)
    $ reverse
    $ lines content

readStatsFile :: FilePath -> Int -> IO [StatsLine]
readStatsFile f minTrials = do
  xs <- readAllStats f
  currTime <- getCurrentTime
  pure
    $ filter (isFromToday currTime)
    $ take minTrials xs

isFromToday :: UTCTime -> StatsLine -> Bool
isFromToday t1 line = utctDay t1 == utctDay t2
  where
    t2 = posixSecondsToUTCTime $ statsTs line

-- | Check whether a file exists. Create it, if not.
checkStatsFile :: FilePath -> IO ()
checkStatsFile f = do
  ok <- doesPathExist f
  if not ok
    then writeFile f ""
    else pure ()

-- | Initialize the game state.
createGame :: TVar Bool -> FilePath -> Int -> Int -> IO Game
createGame isPlaying f lvl minTrials = do
  statsData <- readStatsFile f minTrials
  seqs <- generateSeqs lvl
  return $ Game
    { _visuals    = fst seqs,
      _auditory   = snd seqs,
      _answers    = initAnswers lvl,
      _block      = 0,
      _level      = lvl,
      _isActive   = False,
      _end        = False,
      _answer     = Nothing,
      _screen     = TrialEndScreen,
      _sounds     = allSounds,
      _lastReport = initReport,
      _statsFile  = f,
      _stats      = statsData,
      _trials     = minTrials,
      _playing    = isPlaying,
      _playedSound = False
    }
