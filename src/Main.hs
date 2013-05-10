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
import Data.Maybe(fromMaybe, fromJust)
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

  menu <- vBoxNew False 0
  containerAdd window menu

  gameMenuAction <- actionNew "GMA" "Game"        Nothing Nothing
  prefMenuAction <- actionNew "PMA" "Preferences" Nothing Nothing
  helpMenuAction <- actionNew "HMA" "Help"        Nothing Nothing

  newGameAction <- actionNew "NEWA" "New..." Nothing Nothing
  nameAction <- actionNew "NAMA" "Change Name..." Nothing Nothing
  colorAction <- actionNew "COLA" "Choose Colors..." 
    Nothing $ Just "stockColorPicker"
  aboutAction <- actionNew "ABTA" "About" Nothing Nothing

  agr <- actionGroupNew "AGR"
  mapM_ (actionGroupAddAction agr)
    [ gameMenuAction
    , prefMenuAction
    , helpMenuAction
    ]
  mapM_ (actionGroupAddAction agr)
    [ newGameAction
    , nameAction
    , colorAction
    , aboutAction
    ]

  ui <- uiManagerNew
  uiManagerAddUiFromString ui uiDecl
  uiManagerInsertActionGroup ui agr 0

  menubar <- fmap fromJust $ uiManagerGetWidget ui "/ui/menubar"
  boxPackStart menu menubar PackNatural 0

  on newGameAction actionActivated $ putStrLn "New Game!"
  on nameAction    actionActivated $ putStrLn "Change Name!"
  on colorAction   actionActivated $ putStrLn "Pick Colors!"
  on aboutAction   actionActivated $ putStrLn "ABOUT!"

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
  dialog <- dialogNew
  set dialog
    [ windowResizable       := False
    , windowModal           := True
    , windowSkipTaskbarHint := True
    , windowTitle           := "Your name?"
    ]
  dialogAddButton dialog "OK" ResponseOk
  dialogUpper <- dialogGetUpper dialog
  dialogLabel <- labelNew $ Just "Please enter your name:"
  dialogEntry <- entryNew
  set dialogEntry [ entryText := username ]
  boxPackStartDefaults dialogUpper dialogLabel
  boxPackStartDefaults dialogUpper dialogEntry
  widgetShowAll dialogUpper
  result <- dialogRun dialog
  if result == ResponseOk
  then do
    answer <- entryGetText dialogEntry
    widgetDestroy dialog
    if   answer == ""
    then return "Guest"
    else writeNameToFile answer username fileIsValid
  else widgetDestroy dialog >> return "Guest"

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

-- 'uiDecl' is the declaration for the menubar
uiDecl :: String
uiDecl =
  "<ui>\
  \  <menubar>\
  \    <menu action=\"GMA\">\
  \      <menuitem action=\"NEWA\" />\
  \    </menu>\
  \    <menu action=\"PMA\">\
  \      <menuitem action=\"NAMA\" />\
  \      <menuitem action=\"COLA\" />\
  \    </menu>\
  \    <menu action=\"HMA\">\
  \      <menuitem action=\"ABTA\" />\
  \    </menu>\
  \  </menubar>\
  \</ui>"
