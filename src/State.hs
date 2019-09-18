{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module State
  ( Game (..),
    Answer (..),
    Screen (..),
    StatsLine (..),
    chooseItems,
    generateSeqs,
    getMistakes,
    findAnswers,
    findMatches,
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
import System.Random (newStdGen, randomRIO)
import System.Random.Shuffle (shuffle')

data Answer
  = AuditoryMatch
  | VisualMatch
  | BothMatches
  deriving (Eq, Show)

instance Semigroup Answer where
  AuditoryMatch <> AuditoryMatch = AuditoryMatch
  AuditoryMatch <> VisualMatch = BothMatches
  AuditoryMatch <> BothMatches = BothMatches
  VisualMatch <> VisualMatch = VisualMatch
  VisualMatch <> AuditoryMatch = BothMatches
  VisualMatch <> BothMatches = BothMatches
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
      { visualMistakes :: Int,
        auditoryMistakes :: Int
      }
  deriving (Eq, Show)

deriveJSON defaultOptions ''MistakeReport

instance Semigroup MistakeReport where
  a <> b = MistakeReport
    { visualMistakes = visualMistakes a + visualMistakes b,
      auditoryMistakes = auditoryMistakes a + auditoryMistakes b
    }

instance Monoid MistakeReport where
  mempty = MistakeReport {visualMistakes = 0, auditoryMistakes = 0}

data Screen
  = GameScreen
  | MenuScreen
  | ScoreScreen
  | ProgressScreen
  | TrialEndScreen
  deriving (Eq, Show)

data Game
  = Game
      { _visuals :: Map.Map Int Int,
        -- ^ The visual sequence choosen for the current trial.
        _auditory :: Map.Map Int Int,
        -- ^ The sound sequence choosen for the current trial.
        _answers :: Map.Map Int (Maybe Answer),
        -- ^ The answers given through out the trial.
        _block :: Int,
        -- ^ Current block in a trial.
        _level :: Int,
        -- ^ The value of N in n-back.
        _answer :: Maybe Answer,
        -- ^ The answer given by the user. Nothing if there was no input.
        _screen :: Screen,
        -- ^ The UI that will be shown.
        _isActive :: Bool,
        -- ^ Whether the current stimulus must be visible.
        _end :: Bool,
        -- ^ Whether the trial has ended.
        _sounds :: Map.Map Int SoundFile,
        -- ^ Available sounds to be played.
        _lastReport :: MistakeReport,
        -- ^ The report of the previous trial
        _statsFile :: FilePath,
        -- ^ A file to save the results after each trial.
        _stats :: [StatsLine]
        -- ^ Statistics loaded.
      }
  deriving (Show)

makeLenses ''Game

decideNextLevel :: Int -> MistakeReport -> Int
decideNextLevel lvl report
  | visualMistakes report < 3 && auditoryMistakes report < 3 = lvl + 1
  | totalMistakes > 5 = max 2 (lvl - 1)
  | otherwise = lvl
  where
    totalMistakes = visualMistakes report + auditoryMistakes report

compareAnswers :: Maybe Answer -> Maybe Answer -> MistakeReport
compareAnswers (Just AuditoryMatch) (Just VisualMatch) = MistakeReport {visualMistakes = 0, auditoryMistakes = 1}
compareAnswers (Just AuditoryMatch) (Just BothMatches) = MistakeReport {visualMistakes = 0, auditoryMistakes = 1}
compareAnswers (Just AuditoryMatch) Nothing = MistakeReport {visualMistakes = 0, auditoryMistakes = 1}
compareAnswers (Just VisualMatch) (Just AuditoryMatch) = MistakeReport {visualMistakes = 1, auditoryMistakes = 0}
compareAnswers (Just VisualMatch) (Just BothMatches) = MistakeReport {visualMistakes = 1, auditoryMistakes = 0}
compareAnswers (Just VisualMatch) Nothing = MistakeReport {visualMistakes = 1, auditoryMistakes = 0}
compareAnswers (Just BothMatches) (Just AuditoryMatch) = MistakeReport {visualMistakes = 1, auditoryMistakes = 0}
compareAnswers (Just BothMatches) (Just VisualMatch) = MistakeReport {visualMistakes = 0, auditoryMistakes = 1}
compareAnswers (Just BothMatches) Nothing = MistakeReport {visualMistakes = 1, auditoryMistakes = 1}
compareAnswers Nothing (Just AuditoryMatch) = MistakeReport {visualMistakes = 0, auditoryMistakes = 1}
compareAnswers Nothing (Just VisualMatch) = MistakeReport {visualMistakes = 1, auditoryMistakes = 0}
compareAnswers Nothing (Just BothMatches) = MistakeReport {visualMistakes = 1, auditoryMistakes = 1}
compareAnswers _ _ = MistakeReport {visualMistakes = 0, auditoryMistakes = 0}

getMistakes :: Game -> MistakeReport
getMistakes g = findMistakes correctAnswers guesses
  where
    correctAnswers = findAnswers (g ^. level) (g ^. visuals) (g ^. auditory)
    guesses = Map.elems $ g ^. answers

findMistakes :: [Maybe Answer] -> [Maybe Answer] -> MistakeReport
findMistakes ans guess = mconcat $ zipWith compareAnswers ans guess

findAnswers
  :: Int
  -- ^ The current level.
  -> Map.Map Int Int
  -- ^ Hash map with the visual entries.
  -> Map.Map Int Int
  -- ^ Hash map with the auditory entries.
  -> [Maybe Answer]
findAnswers lvl visualSeries auditorySeries =
  map
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
          else Nothing
    )
    (zip (Map.toList visualSeries) (Map.toList auditorySeries))

randomList
  :: Int
  -- ^ The number of items in the list.
  -> Int
  -- ^ The upper range limit for each item.
  -> IO [Int]
randomList n maxIdx = replicateM n $ randomRIO (0, maxIdx)

chooseItems
  :: Int
  -- ^ The total number of items to pick.
  -> [a]
  -- ^ The list to pick the items.
  -> IO [a]
  -- ^ The list with the picked items.
chooseItems num lst =
  take num . shuffle' lst (length lst) <$> newStdGen

-- | Generate a valid random sequence for the nback trial.
generateSeqs
  :: Int
  -- ^ The level we are in.
  -> IO (Map.Map Int Int, Map.Map Int Int)
  -- ^ The maps with the visual & auditory sequences.
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
fixMatches
  :: Map.Map Int Int
  -- ^ The main series to be changed.
  -> Int
  -- ^ The level for which we are generating.
  -> [Int]
  -- ^ The indices of the series that need adjustment.
  -> Map.Map Int Int
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
findMatches
  :: Int
  -- ^ The current level.
  -> Map.Map Int Int
  -- ^ The stimulus series.
  -> [Int]
  -- ^ The matches for the given series & level.
findMatches lvl series =
  map fst
    $ filter
        (\(i, x) -> i >= lvl && series Map.! (i - lvl) == x)
        (Map.toList series)

-- | Generate the initial answers map with no responses.
initAnswers :: Int -> Map.Map Int (Maybe Answer)
initAnswers lvl = Map.fromList (zip [0 ..] (replicate (numTrials lvl) Nothing))

readStatsFile :: FilePath -> IO [StatsLine]
readStatsFile f = do
  checkStatsFile f
  content <- readFile f
  currTime <- getCurrentTime
  pure
    $ filter (isRecent currTime)
    $ map (either error id . eitherDecode . LBs.pack)
    $ take 15
    $ reverse
    $ lines content

isRecent :: UTCTime -> StatsLine -> Bool
isRecent t1 line = diffUTCTime t1 t2 < twentyFourHours
  where
    t2 = posixSecondsToUTCTime $ statsTs line
    twentyFourHours = 86400

-- | Check whether a file exists. Create it, if not.
checkStatsFile :: FilePath -> IO ()
checkStatsFile f = do
  ok <- doesPathExist f
  if not ok
    then writeFile f ""
    else pure ()

-- | Initialiaze the game state.
createGame :: FilePath -> Int -> IO Game
createGame f lvl = do
  statsData <- readStatsFile f
  seqs <- generateSeqs lvl
  return $ Game
    { _visuals = fst seqs,
      _auditory = snd seqs,
      _answers = initAnswers lvl,
      _block = 0,
      _level = lvl,
      _isActive = False,
      _end = False,
      _answer = Nothing,
      _screen = TrialEndScreen,
      _sounds = allSounds,
      _lastReport = MistakeReport
        { visualMistakes = -1,
          auditoryMistakes = -1
        },
      _statsFile = f,
      _stats = statsData
    }
