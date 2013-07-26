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

import Control.Monad(forM_, when)
import Control.Monad.Trans(liftIO)
import Data.Either(rights)
import Data.IORef(newIORef,readIORef,writeIORef,IORef())
import Data.Maybe(fromMaybe, fromJust, isJust)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import System.Directory(doesFileExist)
import System.Environment(getEnvironment)
import System.IO(openFile, hGetContents, hPutStrLn, IOMode(..), hClose)
import Base hiding (Color, getColor)
import qualified Base(getColor)

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

  boardIO      <- newIORef $ head . rights . return . empty $ 19

  initGUI

  window <- windowNew
  window `on` deleteEvent $ liftIO mainQuit >> return False
  windowSetDefaultIconFromFile "stones.ico"
  windowMaximize window
  set window
    [ windowTitle          := "Konnekzix"
    , windowWindowPosition := WinPosCenter
    ]

  menu <- vBoxNew False 0
  containerAdd window menu

  contents <- hBoxNew False 0
  boardDA <- drawingAreaNew
  deleteMe <- labelNew $ Just "Foo" -- TODO: Change to Menu
  deleteMe2 <- labelNew $ Just "Bar" -- TODO: Change to Chat

  boardDA `on` sizeRequest $ return $ Requisition 1000 1000
  boardDA `on` exposeEvent $ liftIO
    (drawboard boardDA boardIO
      foreColorIO backColorIO blackColorIO whiteColorIO) >> return False

  boxPackStartDefaults menu contents
  boxPackStartDefaults contents boardDA
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
                        Foreground -> writeIORef foreColorIO  color
                        Background -> writeIORef backColorIO  color
                        PieceBlack -> writeIORef blackColorIO color
                        PieceWhite -> writeIORef whiteColorIO color)
    widgetQueueDraw boardDA

  aboutAction `on` actionActivated $ putStrLn "ABOUT!" -- TODO: About action.
                                                       -- Just a dialog.

  widgetShowAll window

  name <- getName
  writeIORef nameIO name

  mainGUI

-- 'drawboard' draws the board.
-- TODO: recall function when colors are changed
drawboard :: DrawingArea -> IORef Board ->
  IORef Color -> IORef Color -> IORef Color -> IORef Color -> IO ()
drawboard da bIO foIO baIO blIO whIO = do
  d <- widgetGetDrawWindow da
  foreColor <- readIORef foIO
  backColor <- readIORef baIO
  blackColor <- readIORef blIO
  whiteColor <- readIORef whIO
  board <- readIORef bIO
  -- Draw Background
  gc <- gcNewWithValues d $ newGCValues { foreground = backColor }
  drawRectangle d gc True 10 10 980 980
  -- Draw Lines
  gcSetValues gc $ newGCValues { foreground = foreColor, lineWidth = 2 }
  drawboardDALines d gc
  -- Draw Stones
  mapM_ (drawStone d gc (blackColor, whiteColor) board)
    [ (x,y) | x <- [1..19], y <- [1..19] ]

drawStone :: DrawableClass d => d -> GC -> (Color, Color) ->
  Board -> (Int,Int) -> IO ()
drawStone d gc (blCol, whCol) b ps@(x,y) = let st = getStone b ps in
  when (isJust st) $ do
    let gcColor = if (Base.getColor . fromJust) st == Black
                    then blCol
                    else whCol
    gcSetValues gc $ newGCValues { foreground = gcColor }
    drawCircle d gc 25 (x*51-10,y*51-10)

drawboardDALines :: DrawWindow -> GC -> IO ()
drawboardDALines d gc = do
  drawSegments d gc
    [ ((x,41),(x,959)) | x <- [41,92..959] ]
  drawSegments d gc
    [ ((41,y),(959,y)) | y <- [41,92..959] ]
  mapM_ (drawCircle d gc 6) $
    let ps = [194,500,806] in [ (x,y) | x <- ps, y <- ps ]

drawCircle :: DrawableClass d => d -> GC -> Int -> (Int, Int) -> IO ()
drawCircle d gc r (x,y) = drawArc d gc True (x-r) (y-r) (2*r) (2*r) 0 (360*64)

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
pickColors :: IO [(ColoredThing, Color)]
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
      let colors = [ (Foreground, fgCol)
                   , (Background, bgCol)
                   , (PieceBlack, pbCol)
                   , (PieceWhite, pwCol)
                   ]
      forM_ colors (\(usage, color) -> do
                     file <- openFile ("col" ++ colToStr usage) WriteMode
                     hPutStrLn file $ encode color
                     hClose file)
      widgetDestroy dialog
      return colors
    else widgetDestroy dialog >> return []

-- helper function for color selection
createColorSelectionArea :: ColoredThing -> IO (HBox, ColorButton)
createColorSelectionArea usage = do
  hBox <- hBoxNew True 0
  label <- labelNew $ Just $ colToStr usage ++ ":"
  color <- getColor usage
  button <- colorButtonNewWithColor color
  boxPackStartDefaults hBox label
  boxPackStartDefaults hBox button
  return (hBox, button)

colToStr :: ColoredThing -> String
colToStr usage = case usage of
                   PieceWhite -> "White Piece"
                   PieceBlack -> "Black Piece"
                   _          -> show usage

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
