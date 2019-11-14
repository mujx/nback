{-# LANGUAGE OverloadedStrings #-}

module Chart
  ( mkChart,
  )
where

import Lens.Micro
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.List (groupBy)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import State (StatsLine (..), calcPerf)


scores :: [StatsLine] -> [(LocalTime, Double, Double)]
scores xs =
  map dayAvg
    $ filter withEnoughEntries
    $ groupBy sameDay xs
  where
    withEnoughEntries :: [StatsLine] -> Bool
    withEnoughEntries x = length x >= 5

    -- | Check if two entries are from the same day.
    sameDay :: StatsLine -> StatsLine -> Bool
    sameDay a b = day' a == day' b
      where
        day' = utctDay . posixSecondsToUTCTime . statsTs

    -- | Compute the average score over the given entries.
    dayAvg :: [StatsLine] -> (LocalTime, Double, Double)
    dayAvg [] = error "An empty list was generated"
    dayAvg ss = (getTime $ head ss, audioAvg ss, visualAvg ss)
      where
        audioAvg  ss' = sum (map (\x -> getAudio x / 100) ss') / (fromIntegral (length ss') :: Double)
        visualAvg ss' = sum (map (\x -> getVisual x / 100) ss') / (fromIntegral (length ss') :: Double)

    getTime :: StatsLine -> LocalTime
    getTime x = utcToLocalTime utc (posixSecondsToUTCTime (statsTs x))

    getScore :: ((Float, Float) -> Float) -> StatsLine -> Double
    getScore fn x = realToFrac (fn ( calcPerf $ statsReport x) * (fromIntegral (statsLevel x) :: Float))

    getAudio :: StatsLine -> Double
    getAudio = getScore fst

    getVisual :: StatsLine -> Double
    getVisual = getScore snd

chart :: [StatsLine] -> Renderable ()
chart xs = toRenderable layout
  where
    audio =
      plot_lines_style . line_color .~ opaque blue
        $ plot_lines_values .~ [[(d, v) | (d, v, _) <- scores xs]]
        $ plot_lines_title .~ "Audio"
        $ def
    visual =
      plot_lines_style . line_color .~ opaque green
        $ plot_lines_values .~ [[(d, v) | (d, _, v) <- scores xs]]
        $ plot_lines_title .~ "Visual"
        $ def
    layout =
      layoutlr_title .~ "Score"
        $ layoutlr_left_axis . laxis_override .~ axisGridHide
        $ layoutlr_right_axis . laxis_override .~ axisGridHide
        $ layoutlr_x_axis . laxis_override .~ axisGridHide
        $ layoutlr_plots .~ [Left (toPlot audio), Right (toPlot visual)]
        $ layoutlr_grid_last .~ False
        $ def

-- | Show a 2D chart with the score over time.
--
--   We show separate lines for the audio & visual score.
mkChart :: [StatsLine] -> IO (PickFn ())
mkChart xs = renderableToFile def "score.svg" (chart xs)
