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
import Graphics.UI.Gtk
import System.Directory(doesFileExist)
import System.Environment(getEnvironment)
import System.IO(openFile, hGetContents, hPutStrLn, IOMode(..))

-- | the 'main' function starts up the GUI and everything else.
main :: IO ()
main = do
  initGUI
  window <- windowNew
  on window deleteEvent $ liftIO mainQuit >> return False
  set window
    [ windowTitle := "Konnekzix"
    , windowWindowPosition := WinPosCenter
    ]
  windowSetDefaultIconFromFile "stones.ico"
  windowMaximize window
  widgetShowAll window
  name <- getName
  putStrLn name -- temporary
  mainGUI

-- 'extractUserName' takes the whole Environment and returns a username.
extractUserName :: [(String,String)] -> String
extractUserName e =
  foldl fromMaybe "" $ map (`lookup` e) ["USERNAME","LOGNAME"]

-- writes the name of the user to a file in the format <logname>:<name>
writeNameToFile :: String -> String -> Bool -> IO String
writeNameToFile name username fileIsValid = do
  file <- openFile "names" (if fileIsValid then AppendMode else WriteMode)
  hPutStrLn file $ username ++ ":" ++ name
  return name

-- if a name for the current user already exists in the "names" file,
-- 'getName' will return that name. Otherwise, it will ask the user for
-- a name.
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

-- 'askForName' opens the dialog in the user can enter their name.
askForName :: String -> Bool -> IO String
askForName username fileIsValid = do
  nameDialog <- dialogNew
  set nameDialog
    [ windowResizable       := False
    , windowModal           := True
    , windowSkipTaskbarHint := True
    , windowTitle           := "Your name?"
    ]
  dialogAddButton nameDialog "OK" ResponseOk
  nameDialogUpper <- dialogGetUpper nameDialog
  nameDialogLabel <- labelNew $ Just "Please enter your name:"
  nameDialogEntry <- entryNew
  set nameDialogEntry [ entryText := username ]
  boxPackStartDefaults nameDialogUpper nameDialogLabel
  boxPackStartDefaults nameDialogUpper nameDialogEntry
  widgetShowAll nameDialogUpper
  result <- dialogRun nameDialog
  if result == ResponseOk
  then do
    answer <- entryGetText nameDialogEntry
    widgetDestroy nameDialog
    if   answer == ""
    then return "Guest"
    else writeNameToFile answer username fileIsValid
  else widgetDestroy nameDialog >> return "Guest"

-- 'searchForName' searches for a name in a string in the format
-- {<username>:<name>\n}
searchForName :: String -> String -> Maybe String
searchForName username xs
  | isValid xs = lookup username $ parse xs
  | otherwise  = Nothing

-- 'isValid' checks, whether a String can be parsed to an association list
isValid :: String -> Bool
isValid = all correct . lines
  where correct = (\(u,n) -> u /= "" && n /= "") . span (/= ':')

-- 'parse' takes a String in the format {<username>:<name>\n} and returns
-- an association list
parse :: String -> [(String,String)]
parse = map parseLn . lines
  where parseLn = fmap tail . span (/= ':')
