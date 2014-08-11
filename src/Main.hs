{-# LANGUAGE ScopedTypeVariables,GADTs,FlexibleContexts #-} -- allows "forall t. Moment t" module Main where
import Graphics.UI.Gtk
import Control.Monad.State
import Data.Maybe
import Reactive.Banana hiding (apply)
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk
import Diagrams.Prelude hiding (stroke, moveTo, scale)
import Diagrams.Transform
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo
import Data.NumInstances.Tuple
import Utility

dir2int :: ScrollDirection -> Int
dir2int ScrollUp = 1
dir2int ScrollDown = -1
dir2int _ = 0

scroll2Double :: ScrollDirection -> Double
scroll2Double s = (1.1) ** (fromIntegral $ dir2int s)

theDia :: Diagram B R2
--theDia = circle 110 # fc green
theDia = connectOutside' (with & headLength .~ (Local 20) & gap .~(Local 10)) "c1" "c2" $ rotateBy (1/3) $ showOrigin $ center $ named "c1" (circle 20 <> text "1") ||| strutX 100 ||| named "c2" (circle 20)

draw :: DrawableClass dc => Diagram B R2 -> dc -> IO( )
draw d dc = do
    renderToGtk dc $ d

redraw :: (DrawWindowClass w) => w -> IO()
redraw w = do
    reg <- drawableGetClipRegion w
    drawWindowInvalidateRegion w reg True

scrollTransfm :: (HasLinearMap v, Fractional (Scalar v), Eq (Scalar v)) => v -> Scalar v -> Transformation v
scrollTransfm v s = scale s $ translation $ (1/s - 1) *^ v

--main function
main = do
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

   -- describe the ractive network
   let networkDescription :: forall t. Frameworks t => Moment t ()
       networkDescription = do

       eExpose <- mevent drawingArea exposeEvent eventWindow
       eScroll <- mevent drawingArea scrollEvent eventScrollDirection
       eMotion <- mevent drawingArea motionNotifyEvent eventCoordinates
       eLeave <- mevent drawingArea leaveNotifyEvent $ return ()
       eButtonPressed <- mevent drawingArea buttonPressEvent $ return ()
       eButtonReleased <- mevent drawingArea buttonReleaseEvent $ return ()
       eKeyPressed <- mevent window keyPressEvent $ eventKeyVal
       eAreaSize <- mevent drawingArea configureEvent $ eventSize

       let eNeedRefresh = (const () <$> eDrag) `union` (const () <$> eScroll)
           bMouseCoords = stepper (0,0) eMotion
           bIsDragDrop = stepper False $ 
             ((pure True) <@ eButtonPressed ) `union` ((pure False) <@ (eButtonReleased `union` eLeave))
           eDrag = filterApply (const <$> bIsDragDrop) $ (flip (-) <$> bMouseCoords) <@> eMotion

           eTranslation = translation . r2 <$> eDrag
           eScrollTransfm = (scrollTransfm . r2 <$> bMouseCoords) <@> (scroll2Double <$> eScroll)
           eZoomTransfm = never
           eTransfm = eTranslation `union` eScrollTransfm `union` eZoomTransfm

           bTransfm = accumB mempty $ (<>) <$> eTransfm 
           --bDiagram = accumB theDia (transform <$> eTransfm)
           bDiagram = (transform <$> bTransfm) <*> pure theDia
        
           --use this only for scrolling (as it's the behavior)
           eAreaCenter = (0.5 *^) . both ( fromIntegral :: Int -> Double ) <$> eAreaSize
           

       reactimate ( draw <$> bDiagram <@> eExpose )
       reactimate ( pure ( redraw =<< widgetGetDrawWindow drawingArea ) <@ eNeedRefresh)
       --reactimate ( ( \x-> print $ fromMaybe (keyName x) (show <$> keyToChar x) ) <$> eKeyPressed )
       --reactimate ( print <$> bAreaCenter <@ eAreaSize )
       sink window [ windowTitle :== (show <$> bMouseCoords) ]
       return ()

   network <- compile networkDescription    
   actuate network
   mainGUI

