--

module Data.Tsolitaire.Interface
  ( start
  )
where

import qualified Data.Tsolitaire.Interface.Game as Game
import qualified Data.Tsolitaire.Interface.MeasureRate as MeasureRate
import qualified Data.Tsolitaire.Interface.Simulate as Simulate
import Data.Tsolitaire.Interface.Util
import System.IO


start :: IO ()
start = do
  func <- inputMode
  flushStrLn ""
  func

inputMode :: IO (IO ())
inputMode = do
  flushStr $ colorInput "<?> Mode -> "
  input <- getLine
  case input of
    "g" -> return Game.start
    "s" -> return Simulate.start
    "m" -> return MeasureRate.start
    _ -> do
      flushStrLn $ colorError "@ No such mode."
      inputMode