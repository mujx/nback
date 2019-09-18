{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import Audio (playSound)
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import Brick.Types
  ( BrickEvent (..),
    Widget,
  )
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as Core
import Constants (restDuration, stimulusDuration)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBs
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Version (showVersion)
import Graphics.Vty (black, blue, defaultConfig, green, mkVty, red, white, yellow)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import Paths_nback (version)
import State
  ( Answer (..),
    Game (..),
    MistakeReport (..),
    Screen (..),
    StatsLine (..),
    createGame,
    decideNextLevel,
    getMistakes,
  )
import System.Directory

makeLenses ''Game

drawUI :: Game -> [Widget ()]
drawUI g =
  [ C.center
      $ Core.vBox
          [ drawMain g,
            Core.padTop (T.Pad 1) (Core.hBox [drawLevelInfo g, drawControls])
          ]
  ]

drawLevelInfo :: Game -> Widget ()
drawLevelInfo g =
  Core.padLeft (T.Pad 1)
    $ Core.vBox
        [ Core.str ("Level: " <> show (g ^. level)),
          Core.str
            ( "Block: "
                <> show (g ^. block)
                <> "/"
                <> show (length $ g ^. auditory)
            )
        ]

drawControls :: Widget Name
drawControls =
  C.hCenter
    $ Core.vBox
        [ Core.txt "Audio:  A",
          Core.txt "Visual: L"
        ]

drawMain :: Game -> Widget ()
drawMain g =
  case g ^. screen of
    GameScreen -> drawGrid g
    TrialEndScreen -> drawInterlude g
    _ -> error "not implemented yet"

pastTrials :: Game -> Widget ()
pastTrials g =
  if null (g ^. stats)
    then Core.txt ""
    else
      C.hCenter $ Core.withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (Core.txt " Recent trials (24h)")
        $ Core.padAll 1
        $ Core.vBox
            [ Core.padTopBottom 1
                $ Core.vBox [Core.withAttr (lineColor v) $ stat v | v <- g ^. stats]
            ]
  where
    space = " "
    lineColor v =
      let nextLvl = decideNextLevel (statsLevel v) (statsReport v)
          currentLvl = statsLevel v
       in if nextLvl > currentLvl
            then "greenLine"
            else
              if nextLvl == currentLvl
                then "yellowLine"
                else "redLine"
    stat v =
      Core.str
        ( space
            <> formatTime defaultTimeLocale "%R %D" (posixSecondsToUTCTime $ statsTs v)
            <> ": L"
            <> show (statsLevel v)
            <> space
            <> ("Audio (" <> show (auditoryMistakes (statsReport v)) <> ")")
            <> space
            <> ("Visual (" <> show (visualMistakes (statsReport v)) <> ")")
            <> space
        )

drawInterlude :: Game -> Widget ()
drawInterlude g =
  Core.vBox
    [ C.center $ Core.withBorderStyle BS.unicodeBold
        $ B.borderWithLabel title
        $ Core.padAll 1
        $ Core.vBox
        $ reportLines
          <> [ Core.padTopBottom 1
                 $ Core.vBox
                     [ Core.str $ "Next level: " <> show (g ^. level),
                       Core.str "Press <SPACE> to continue"
                     ]
             ],
      pastTrials g
    ]
  where
    visualErrors :: Int
    visualErrors = visualMistakes (g ^. lastReport)
    auditoryErrors :: Int
    auditoryErrors = auditoryMistakes (g ^. lastReport)
    title :: Widget ()
    title =
      if visualErrors == -1 && auditoryErrors == -1
        then Core.txt " Trial "
        else Core.txt " End of Trial "
    reportLines :: [Widget ()]
    reportLines =
      if visualErrors == -1 && auditoryErrors == -1
        then []
        else
          [ Core.str $ "Visual errors:   " <> show visualErrors,
            Core.str $ "Auditory errors: " <> show auditoryErrors
          ]

drawGrid :: Game -> Widget ()
drawGrid g =
  Core.vBox
    [ Core.hBox [drawGridCell (0 + x) active draw | x <- [0 .. 2]],
      Core.hBox
        [ drawGridCell 3 active draw,
          drawGridCell (-1) active draw, -- The central cell is not used.
          drawGridCell 4 active draw
        ],
      Core.hBox [drawGridCell (5 + x) active draw | x <- [0 .. 2]]
    ]
  where
    -- | The index of the active cell.
    active :: Int
    active =
      if g ^. block < Map.size (g ^. visuals)
        then
          fromMaybe
            (error "failed at drawGrid")
            (Map.lookup (g ^. block) (g ^. visuals))
        else -1
    -- | Whether we can draw the active cell.
    draw :: Bool
    draw = g ^. isActive

drawGridCell
  :: Int
  -- ^ The index of the block we're currently rendering.
  -> Int
  -- ^ The index of the active block
  -> Bool
  -- ^ Whether we are in a stimulus phase.
  -> Widget ()
drawGridCell (-1) _ _ = drawCentralCell
drawGridCell idx activeIdx drawActive =
  if idx == activeIdx && drawActive
    then drawActiveCell
    else drawEmptyCell

emptySpace :: Widget ()
emptySpace = Core.fill ' '

drawActiveCell, drawEmptyCell, drawCentralCell :: Widget ()
drawActiveCell = Core.withAttr "activeCell" box
drawEmptyCell = Core.withAttr "emptyCell" box
drawCentralCell = Core.withAttr "centralCell" $ C.center (Core.str "X")

box :: Widget ()
box = Core.vBox [emptySpace, emptySpace]

handleEvent :: Game -> T.BrickEvent Name ClockEvent -> T.EventM Name (T.Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = M.continue $ registerAnswer (Just AuditoryMatch) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = M.continue $ registerAnswer (Just VisualMatch) g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = M.continue $ startTrial g
handleEvent g (AppEvent Play) =
  case g ^. screen of
    GameScreen ->
      if g ^. end
        then prepareNextLevel g
        else do
          void $ liftIO $ forkIO $ when (hasEnoughBlocks g) (playRandomSound g)
          M.continue $ showNextBlock g
    _ -> M.continue g
handleEvent g (AppEvent Stop) =
  case g ^. screen of
    GameScreen ->
      if g ^. end
        then prepareNextLevel g
        else M.continue $ clearBlock g
    _ -> M.continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = M.halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = M.halt g
handleEvent g _ = M.continue g

startTrial :: Game -> Game
startTrial g = case g ^. screen of
  TrialEndScreen -> g & (screen .~ GameScreen) & (end .~ False)
  _ -> g

hasEnoughBlocks :: Game -> Bool
hasEnoughBlocks g = g ^. block < Map.size (g ^. visuals)

prepareNextLevel :: Game -> T.EventM n (T.Next Game)
prepareNextLevel g = do
  ts <- liftIO getPOSIXTime
  let statLine = mkStatsLine ts g
  liftIO $ saveTrialStats g statLine
  g' <- liftIO $ updateGameStatus g
  M.continue
    ( g'
        & (screen .~ TrialEndScreen)
        & (stats .~ take 15 (statLine : (g ^. stats)))
    )

mkStatsLine :: POSIXTime -> Game -> StatsLine
mkStatsLine ts g =
  StatsLine
    { statsTs = ts,
      statsLevel = g ^. level,
      statsReport = getMistakes g
    }

saveTrialStats :: Game -> StatsLine -> IO ()
saveTrialStats g line =
  appendFile
    (g ^. statsFile)
    (LBs.unpack (encode line <> "\n"))

playRandomSound :: Game -> IO ()
playRandomSound g =
  case Map.lookup pos (g ^. sounds) of
    (Just soundFile) -> playSound soundFile
    Nothing -> error ("playRandomSound: Cannot play non existent file at pos: " <> show pos)
  where
    pos =
      fromMaybe
        (error "playRandomSound: Non existent position in the sounds map")
        (Map.lookup (g ^. block) (g ^. auditory))

updateGameStatus :: Game -> IO Game
updateGameStatus g = nextGame <&> (lastReport .~ report)
  where
    nextGame = createGame (g ^. statsFile) nextLevel
    nextLevel = decideNextLevel (g ^. level) report
    report = getMistakes g

theAttrMap :: A.AttrMap
theAttrMap =
  A.attrMap V.defAttr
    [ ("activeCell", bg blue),
      ("emptyCell", bg black),
      ("centralCell", bg black `V.withStyle` V.bold),
      ("gridBorder", white `on` black),
      ("greenLine", green `on` black),
      ("redLine", red `on` black),
      ("yellowLine", yellow `on` black)
    ]

-- | Set the answer for the block.
--   If an answer has been set try to fuse it with the existing one.
registerAnswer :: Maybe Answer -> Game -> Game
registerAnswer guess g =
  g & (answer .~ ans)
    & (answers .~ newAnswers)
  where
    key = (g ^. block) - 1
    ans = guess <> (g ^. answer)
    newAnswers = Map.insert key ans (g ^. answers)

-- | Show the next pair of stimulus.
showNextBlock :: Game -> Game
showNextBlock g =
  if g ^. block >= Map.size (g ^. visuals)
    then g & (isActive .~ False) & (end .~ True) -- The trial is over if we go past the number of blocks.
    else g & (isActive .~ True)

-- | Make the block inactive.
clearBlock :: Game -> Game
clearBlock g =
  g
    & (isActive .~ False)
    & (answer .~ Nothing)
    & (block .~ (g ^. block + 1))

-- Placeholder for the moment.
type Name = ()

app :: M.App Game ClockEvent Name
app =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.neverShowCursor,
      M.appHandleEvent = handleEvent,
      M.appStartEvent = return,
      M.appAttrMap = const theAttrMap
    }

-- App custom Events
--
-- Events used by the external clock to adjust the UI.
data ClockEvent
  = Play
  | Stop
  deriving (Show)

data CliOpts
  = CliOpts
      { optFile :: String,
        optLevel :: Int
      }

cliOpts :: FilePath -> Opt.Parser CliOpts
cliOpts defDataPath =
  CliOpts
    <$> Opt.strOption
          ( Opt.long "stat-file"
              <> Opt.short 'f'
              <> Opt.help "File to save trial's summary"
              <> Opt.metavar "FILE"
              <> Opt.showDefault
              <> Opt.value defDataPath
          )
    <*> Opt.option Opt.auto
          ( Opt.long "level"
              <> Opt.short 'l'
              <> Opt.help "Specify the N-Back level to start"
              <> Opt.metavar "LEVEL"
              <> Opt.showDefault
              <> Opt.value 2
          )

opts :: FilePath -> Opt.ParserInfo CliOpts
opts defDataPath =
  Opt.info
    (cliOpts defDataPath <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.progDesc "The N-Back game"
        <> Opt.header ("nback :: v" <> showVersion version)
    )

main :: IO ()
main = do
  defDataPath <- getXdgDirectory XdgData "nback"
  createDirectoryIfMissing True defDataPath
  parsedOpts <- Opt.execParser (opts (defDataPath <> "/trials.log"))
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan Play
    threadDelay $ stimulusDuration * 1000
    writeBChan chan Stop
    threadDelay $ restDuration * 1000
  gameState <- createGame (optFile parsedOpts) (optLevel parsedOpts)
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  M.customMain initialVty buildVty (Just chan) app gameState >> pure ()
