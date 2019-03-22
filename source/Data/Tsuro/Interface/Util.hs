--


module Data.Tsuro.Interface.Util where

import System.Console.Pretty
import System.IO


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