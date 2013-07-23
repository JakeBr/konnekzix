{-# LANGUAGE TupleSections #-}
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

import Control.Monad(forM_)
import Control.Monad.Trans(liftIO)
import Data.IORef(newIORef,readIORef,writeIORef,IORef())
import Data.Maybe(fromMaybe, fromJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import System.Directory(doesFileExist)
import System.Environment(getEnvironment)
import System.IO(openFile, hGetContents, hPutStrLn, IOMode(..), hClose)

-- some type I need for coloring
data ColoredThing = Foreground
                  | Background
                  | PieceWhite
                  | PieceBlack
                  deriving Show

-- | the 'main' function starts up the GUI and everything else.
main :: IO ()
main = do
  foreColor    <- getColor Foreground
  backColor    <- getColor Background
  blackColor   <- getColor PieceBlack
  whiteColor   <- getColor PieceWhite
  foreColorIO  <- newIORef foreColor
  backColorIO  <- newIORef backColor
  blackColorIO <- newIORef blackColor
  whiteColorIO <- newIORef whiteColor
  nameIO       <- newIORef ""

  initGUI

  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  windowSetDefaultIconFromFile "stones.ico"
  set window
    [ windowTitle          := "Konnekzix"
    , windowWindowPosition := WinPosCenter
    , windowDefaultWidth   := 1920
    , windowDefaultHeight  := 1080
    ]

  menu <- vBoxNew False 0
  containerAdd window menu

  contents <- hBoxNew False 0
  board <- drawingAreaNew
  deleteMe <- labelNew $ Just "Foo" -- TODO: Change to Menu
  deleteMe2 <- labelNew $ Just "Bar" -- TODO: Change to Chat

  board `on` sizeRequest $ return $ Requisition 1000 1000
  board `on` exposeEvent $ liftIO
    (drawBoard board foreColorIO backColorIO blackColorIO whiteColorIO) >>
    return False

  boxPackStartDefaults menu contents
  boxPackStartDefaults contents board
  boxPackStartDefaults contents deleteMe
  boxPackStartDefaults contents deleteMe2

  gameMenuAction <- actionNew "GMA" "Game"        Nothing Nothing
  prefMenuAction <- actionNew "PMA" "Preferences" Nothing Nothing
  helpMenuAction <- actionNew "HMA" "Help"        Nothing Nothing

  newGameAction <- actionNew "NEWA" "New..." Nothing Nothing
  nameAction <- actionNew "NAMA" "Change Name..." Nothing Nothing
  colorAction <- actionNew "COLA" "Choose Colors..."
    Nothing $ Just "stockColorPicker"
  aboutAction <- actionNew "ABTA" "About" Nothing $ Just "stockAbout"

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

  newGameAction `on` actionActivated $ putStrLn "New Game!"
  nameAction    `on` actionActivated $ newName nameIO
  colorAction   `on` actionActivated $ do
    newColors <- pickColors
    forM_ newColors (\(usage, color) ->
                      case usage of
                        "Foreground" -> writeIORef foreColorIO  color
                        "Background" -> writeIORef backColorIO  color
                        "PieceBlack" -> writeIORef blackColorIO color
                        "PieceWhite" -> writeIORef whiteColorIO color
                        _            -> return ())
  aboutAction `on` actionActivated $ putStrLn "ABOUT!" -- TODO: About action.
                                                       -- Just a dialog.

  widgetShowAll window

  name <- getName
  writeIORef nameIO name

  putStrLn name -- temporary

  mainGUI

-- 'drawBoard' draws the Board.
-- TODO: recall function when colors are changed
drawBoard :: DrawingArea ->
  IORef Color -> IORef Color -> IORef Color -> IORef Color -> IO ()
drawBoard da foIO baIO blIO whIO = do
  d <- widgetGetDrawWindow da
  foreColor <- readIORef foIO
  backColor <- readIORef baIO
  blackColor <- readIORef blIO
  whiteColor <- readIORef whIO
  gc <- gcNewWithValues d $ newGCValues { foreground = backColor }
  drawRectangle d gc True 10 10 980 980
  gcSetValues gc $ newGCValues { foreground = foreColor, lineWidth = 2 }
  drawBoardLines d gc
  -- TODO: Stones

drawBoardLines :: DrawWindow -> GC -> IO ()
drawBoardLines d gc = do
  drawSegments d gc
    [ ((x,41),(x,959)) | x <- [41,92..959] ]
  drawSegments d gc
    [ ((41,y),(959,y)) | y <- [41,92..959] ]
  mapM_ (drawCircle 6) $ let ps = [194,500,806] in [ (x,y) | x <- ps, y <- ps ]
  where drawCircle r (x,y) =
          drawArc d gc True (x-r) (y-r) (2*r) (2*r) 0 (360*64)

-- 'extractUserName' takes the whole Environment and returns a username.
extractUserName :: [(String,String)] -> String
extractUserName e =
  foldl fromMaybe "" $ map (`lookup` e) ["USERNAME","LOGNAME"]

-- writes the name of the user to a file in the format <logname>:<name>
writeNameToFile :: String -> String -> Bool -> IO String
writeNameToFile name username fileIsValid = do
  file <- openFile "names" (if fileIsValid then AppendMode else WriteMode)
  hPutStrLn file $ username ++ ":" ++ name
  hClose file
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
      maybe (askForName username (isValidNamesFile names)) return name
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
    if answer == ""
      then return "Guest"
      else writeNameToFile answer username fileIsValid
    else widgetDestroy dialog >> return "Guest"

-- 'searchForName' searches for a name in a string in the format
-- {<username>:<name>\n}
searchForName :: String -> String -> Maybe String
searchForName username xs
  | isValidNamesFile xs = lookup username $ parseNames xs
  | otherwise  = Nothing

-- 'newName' asks the user for a new name and stores it. NOTE: All old
-- names will be deleted.
newName :: IORef String -> IO ()
newName nameIO = do
  prevName <- readIORef nameIO
  name <- askForName prevName False
  writeIORef nameIO name


-- 'isValidNamesFile' checks, whether a String can be parseNamesd to an
-- association list
isValidNamesFile :: String -> Bool
isValidNamesFile = all correct . lines
  where correct = (\(u,n) -> u /= "" && n /= "") . span (/= ':')

-- 'parseNames' takes a String in the format {<username>:<name>\n} and returns
-- an association list
parseNames :: String -> [(String,String)]
parseNames = map parseNamesLn . lines
  where parseNamesLn = fmap tail . span (/= ':')

getColor :: ColoredThing -> IO Color
getColor usage = do
  let path = "col" ++ show usage
  let defaultColor = defaultColorFor usage
  colorSpecified <- doesFileExist path
  if colorSpecified
    then do
      file <- openFile path ReadMode
      color <- hGetContents file
      let answer = parseColor $ init color
      let closeAndReturn x = x `seq` hClose file >> return x
      closeAndReturn $ fromMaybe defaultColor answer
    else
      return defaultColor

-- 'defaultColorFor' returns the default color for a given purpose
defaultColorFor :: ColoredThing -> Color
defaultColorFor usage = case usage of
                          Foreground -> Color  5000  5000  5000
                          Background -> Color 56831 45568 23551
                          PieceBlack -> Color     0     0     0
                          PieceWhite -> Color 65535 65535 65535

-- 'parseColor' takes a String of format "["a","b","c"]" and returns
-- Just the Color, if the String is valid. Otherwise, it returns nothing.
parseColor :: String -> Maybe Color
parseColor s = case reads s of
                 [([r,g,b],"")] -> Just $ Color r g b
                 _              -> Nothing

-- 'pickColors' opens a dialog so that the user can pick custom colors
pickColors :: IO [(String, Color)]
pickColors = do
  dialog <- dialogNew
  set dialog
    [ windowResizable       := False
    , windowModal           := True
    , windowSkipTaskbarHint := True
    , windowTitle           := "Pick colors"
    ]
  dialogAddButton dialog "OK" ResponseOk
  dialogUpper <- dialogGetUpper dialog
  (sAreaFG, sButtonFG) <- createColorSelectionArea Foreground
  (sAreaBG, sButtonBG) <- createColorSelectionArea Background
  (sAreaPB, sButtonPB) <- createColorSelectionArea PieceBlack
  (sAreaPW, sButtonPW) <- createColorSelectionArea PieceWhite
  mapM_ (boxPackStartDefaults dialogUpper)
    [ sAreaFG
    , sAreaBG
    , sAreaPB
    , sAreaPW
    ]
  widgetShowAll dialogUpper
  result <- dialogRun dialog
  if result == ResponseOk
    then do
      fgCol <- colorButtonGetColor sButtonFG
      bgCol <- colorButtonGetColor sButtonBG
      pbCol <- colorButtonGetColor sButtonPB
      pwCol <- colorButtonGetColor sButtonPW
      let colors = [ ("Foreground", fgCol)
                   , ("Background", bgCol)
                   , ("PieceBlack", pbCol)
                   , ("PieceWhite", pwCol)
                   ]
      forM_ colors (\(usage, color) -> do
                     file <- openFile ("col" ++ usage) WriteMode
                     hPutStrLn file $ encode color
                     hClose file)
      widgetDestroy dialog
      return colors
    else widgetDestroy dialog >> return []

-- helper function for color selection
createColorSelectionArea :: ColoredThing -> IO (HBox, ColorButton)
createColorSelectionArea usage = do
  hBox <- hBoxNew True 0
  label <- labelNew $ Just $
    case usage of
      PieceWhite -> "White Piece"
      PieceBlack -> "Black Piece"
      _          -> show usage
    ++ ":"
  color <- getColor usage
  button <- colorButtonNewWithColor color
  boxPackStartDefaults hBox label
  boxPackStartDefaults hBox button
  return (hBox, button)

-- 'encode' converts a Color to a String
encode :: Color -> String
encode color = "[" ++ first ++ "," ++ second ++ "," ++ third ++ "]"
  where (first , rest ) = break (== ' ') . drop 6 $ show color
        (second, rest') = break (== ' ') . tail $ rest
        third           = tail rest'


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
