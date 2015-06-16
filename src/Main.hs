{-# LANGUAGE ScopedTypeVariables,GADTs,FlexibleContexts,TypeSynonymInstances #-} -- allows "forall t. Moment t"
module Main where
import Prelude hiding ( readFile, hGetContents )

import Control.Lens( over, both )

import Data.Maybe
import Data.NumInstances.Tuple
import Data.Text.Lazy.IO ( readFile, hGetContents )
import Data.Text.Lazy
import Data.Graph.Inductive
import Data.Graph.Inductive.PatriciaTree( Gr )
import Data.GraphViz.Parsing( parse, runParser, Parse )
import Data.GraphViz (dotToGraph, Attributes)
import Data.GraphViz.Types.Canonical( DotGraph )

import System.IO hiding (hGetContents)
import System.Environment

import Graphics.UI.Gtk

import Reactive.Banana hiding (apply)
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk

import Diagrams.Prelude hiding (stroke, moveTo, scale)
import Diagrams.Transform
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo

import GVis.GraphAlgo
import GVis.GVis( graphToDia, convertGraph, TwoDRender)

import Utility
import Options

instance TwoDRender B

dir2int :: ScrollDirection -> Int
dir2int ScrollUp = 1
dir2int ScrollDown = -1
dir2int _ = 0

scroll2Double :: ScrollDirection -> Double
scroll2Double s = 1.1 ** fromIntegral ( dir2int s )

keyToZoom :: Char -> Double
keyToZoom '+' = 1.1
keyToZoom '-' = 1.1 ** (-1)
keyToZoom x = error $ "Illegal key for zoom: " ++ show x

draw :: DrawableClass dc => Diagram B -> dc -> IO( )
draw d dc = renderToGtk dc d

redraw :: (DrawWindowClass w) => w -> IO()
redraw w = do
    reg <- drawableGetClipRegion w
    drawWindowInvalidateRegion w reg True

scrollTransfm :: (HasLinearMap v, Fractional n, Eq n) => v n -> n -> Transformation v n
scrollTransfm v s = scale s $ translation $ (1/s - 1) *^ v

parseGraph :: Text -> Either String (Gr Attributes Attributes)
parseGraph file =
  (dotToGraph <$>) $ fst $ runParser parser file
  where
    parser = parse :: Parse ( DotGraph Node )

--main function
main = do
   args <- getArgs
   let cfg = parseArgs args
   -- the GTK's init gui
   initGUI
   -- create a new window
   window <- windowNew
   -- with a drawing area
   drawingArea <- drawingAreaNew
   containerAdd window drawingArea
   -- on exit
   _ <- on window deleteEvent (liftIO ( mainQuit >> return False ))

   -- enable mouse enents handling
   widgetAddEvents drawingArea [PointerMotionMask,ButtonPressMask,ButtonReleaseMask,LeaveNotifyMask]

   -- show the window
   widgetShowAll window

   initialSize <- widgetGetSize drawingArea
   let initialCenter = (over both ( (0.5*) . fromIntegral) ) initialSize

   handle <- if isJust (filename cfg) 
             then openFile (fromJust $ filename cfg) ReadMode
             else return stdin
  
   file <- hGetContents handle
   let graph0 = (\(Right x) -> x) $ parseGraph file :: Gr Attributes Attributes

   let graph = convertGraph $ prepareGraph graph0

   -- describe the reactive network
   let networkDescription :: forall t. Frameworks t => Moment t ()
       networkDescription = do

         eExpose <- mevent drawingArea exposeEvent eventWindow
         eScroll <- mevent drawingArea scrollEvent eventScrollDirection
         eMotion <- mevent drawingArea motionNotifyEvent eventCoordinates
         eLeave <- mevent drawingArea leaveNotifyEvent $ return ()
         eButtonPressed <- mevent drawingArea buttonPressEvent $ return ()
         eButtonReleased <- mevent drawingArea buttonReleaseEvent $ return ()
         eKeyPressed <- mevent window keyPressEvent eventKeyVal
         eAreaSize <- mevent drawingArea configureEvent eventSize

         let eNeedRefresh = eTransfm
             bMouseCoords = stepper (0,0) eMotion
             bIsDragDrop = stepper False $
               ( pure True <@ eButtonPressed ) `union` ( pure False <@ (eButtonReleased `union` eLeave))
             eDrag = filterApply (const <$> bIsDragDrop) $ (flip (-) <$> bMouseCoords) <@> eMotion

             eTranslation = translation . r2 <$> eDrag
             eScrollTransfm = (scrollTransfm . r2 <$> bMouseCoords) <@> (scroll2Double <$> eScroll)
             eZoomTransfm = ( scrollTransfm . r2 <$> bAreaCenter <@> ) $
                      ( keyToZoom <$> ) $
                      filterE ( liftA2 (||) (=='+') (=='-') ) $
                      filterJust $ keyToChar <$> eKeyPressed

             eTransfm = eTranslation `union` eScrollTransfm `union` eZoomTransfm
             bTransfm = accumB ( scalingY (-1) <> translation ( r2 initialCenter) ) $ (<>) <$> eTransfm

             bDiagram = (transform <$> bTransfm) <*> pure ( graphToDia graph )

             bAreaSize = stepper initialSize eAreaSize
             bAreaCenter = ( over both ( (0.5*) .fromIntegral ) ) <$> bAreaSize

         reactimate ( draw <$> bDiagram <@> eExpose )
         reactimate ( ( redraw =<< widgetGetDrawWindow drawingArea ) <$ eNeedRefresh)
         reactimate ( ( \x-> print $ fromMaybe (show $ keyName x) ( show <$> keyToChar x) ) <$> eKeyPressed )
         sink window [ windowTitle :== (show <$> bMouseCoords) ]
         return ()

   network <- compile networkDescription
   actuate network
   mainGUI
