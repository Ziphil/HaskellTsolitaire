--


module Data.Tsolitaire.Interface.Util 
  ( colorMessage
  , colorError
  , colorInput
  , colorTile
  , colorHand
  , colorTurn
  , flushStrLn
  , flushStr
  , cursorUpLine
  , cursorDownLine
  , clearLine
  , prepareProgress
  , updateProgress
  )
where

import System.Console.Pretty
import System.IO
import Text.Printf
import Ziphil.Util.List


colorMessage :: Pretty a => a -> a
colorMessage = color Cyan

colorError :: Pretty a => a -> a
colorError = color Red

colorInput :: Pretty a => a -> a
colorInput = color Yellow

colorTile :: Pretty a => a -> a
colorTile = style Reverse

colorHand :: Pretty a => a -> a
colorHand = style Reverse . color Yellow

colorTurn :: Pretty a => a -> a
colorTurn = style Underline . color Black

flushStrLn :: String -> IO ()
flushStrLn string = putStrLn string >> hFlush stdout

flushStr :: String -> IO ()
flushStr string = putStr string >> hFlush stdout

cursorUpLine :: Int -> IO ()
cursorUpLine size = flushStr $ "\x1b[" ++ show size ++ "F"

cursorDownLine :: Int -> IO ()
cursorDownLine size = flushStr $ "\x1b[" ++ show size ++ "E"

clearLine :: IO ()
clearLine = flushStr $ "\x1b[2K"

-- プログレスバーを表示する準備をします。
-- プログレスバーの更新を始める前に必ずこの関数を呼んでください。
prepareProgress :: IO ()
prepareProgress = do
  flushStrLn ""
  updateProgress 0

-- プログレスバーの更新をします。
updateProgress :: Double -> IO ()
updateProgress progress = do
  cursorUpLine 1
  clearLine
  flushStrLn $ colorMessage $ "@ Progress: " ++ makeBar 30 progress ++ " " ++ printf "%5.2f" (progress * 100) ++ "%"

makeBar :: Int -> Double -> String
makeBar maxLength progress = interpose "[" "]" $ replicate barLength '#' ++ replicate (maxLength - barLength) '-'
  where
    barLength = truncate $ progress * fromIntegral maxLength