--------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) Jakob Brünker, 2013
-- License     : BSD3
--
-- Maintainer  : jake.bruenker@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module contains the main function, including GUI. 
-- Compiling will result in an executable file.
--
--------------------------------------------------------------------------------

import Control.Monad.Trans(liftIO)
import Data.Maybe(fromMaybe)
import Data.Text(unpack, pack, split)
import Graphics.UI.Gtk
import System.Directory(doesFileExist)
import System.Environment(getEnvironment)
import System.IO(openFile, hGetContents, hPutStrLn, IOMode(..))

main :: IO ()
main = do
  initGUI
  window <- windowNew
  on window deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  name <- getName
  putStrLn name -- temporary
  mainGUI

-- 'extractUserName' takes the whole Environment and returns a username.
extractUserName :: [(String,String)] -> String
extractUserName e =
  foldl fromMaybe "" $ map (`lookup` e) ["USERNAME","LOGNAME"]

writeNameToFile :: String -> String -> Bool -> IO String
writeNameToFile name username fileIsValid = do
  file <- openFile "names" (if fileIsValid then AppendMode else WriteMode)
  hPutStrLn file $ username ++ ":" ++ name
  return name

getName :: IO String
getName = do
  username <- fmap extractUserName getEnvironment
  namesExist <- doesFileExist "names"
  if namesExist
  then do 
    hNames <- openFile "names" ReadMode
    names <- hGetContents hNames
    let name = searchForName username names
    maybe (askForName username (isValid names)) return name
  else askForName username False

askForName :: String -> Bool -> IO String
askForName username fileIsValid = do
  nameDialog <- dialogNew
  set nameDialog
    [ windowResizable       := False
    , windowModal           := True
    , windowSkipTaskbarHint := True
    ]
  dialogAddButton nameDialog "OK" ResponseOk
  nameDialogUpper <- dialogGetUpper nameDialog
  nameDialogLabel <- labelNew $ Just "Please enter your name:"
  boxPackStartDefaults nameDialogUpper nameDialogLabel
  nameDialogEntry <- entryNew
  set nameDialogEntry [ entryText := username ]
  boxPackStartDefaults nameDialogUpper nameDialogEntry
  widgetShowAll nameDialogUpper
  result <- dialogRun nameDialog
  if result == ResponseOk
  then do
    answer <- entryGetText nameDialogEntry
    widgetDestroy nameDialog
    if   answer == ""
    then return "guest"
    else writeNameToFile answer username fileIsValid
  else widgetDestroy nameDialog >> return "guest"

searchForName :: String -> String -> Maybe String
searchForName username xs
  | isValid xs = lookup username $ parse xs
  | otherwise  = Nothing

isValid :: String -> Bool
isValid = all correct . lines
  where correct xs = (length . split (== ':') . pack $ xs) == 2

parse :: String -> [(String,String)]
parse = map parseLn . lines
  where parseLn xs =
          (\[u,n] -> (unpack u,unpack n)) (split (== ':') . pack $ xs)
