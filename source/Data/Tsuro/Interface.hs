--


module Data.Tsuro.Interface
  ( start
  )
where

import qualified Data.Tsuro.Interface.Game as Game
import qualified Data.Tsuro.Interface.Simulate as Simulate
import Data.Tsuro.Interface.Util
import System.Console.Pretty
import System.IO


start :: IO ()
start = do
  mode <- inputMode
  flushStrLn ""
  case mode of
    "g" -> Game.start
    "s" -> Simulate.start

inputMode :: IO String
inputMode = do
  flushStr $ colorInput "<?> Mode -> "
  input <- getLine
  case input of
    "g" -> return "g"
    "s" -> return "s"
    _ -> do
      flushStrLn $ colorError "@ No such mode."
      inputMode