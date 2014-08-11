{-# LANGUAGE ScopedTypeVariables,GADTs #-} -- allows "forall t. Moment t"
module Reactive.Banana.Gtk where
import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

data Prop' t w = forall a. (Attr w a) :== Behavior t a

sink :: Frameworks t =>
    w -> [Prop' t w] -> Moment t ()
sink widget = mapM_ sink1
    where sink1 ( attr :== b ) = do
            x <- initial b
            liftIOLater $ set widget [ attr := x ]
            e <- changes b
            reactimate' $ (fmap $ \x -> (set widget [attr := x] >> return ())) <$> e

mevent :: (Frameworks t, WidgetClass w) => 
    w -> 
    Signal w (EventM e Bool) -> 
    EventM e a ->
    Moment t (Event t a)
mevent w s f = do
    (addH, runH) <- liftIO newAddHandler
    _ <- liftIO ( on w s ( f >>= liftIO . runH >> return False ) )
    fromAddHandler addH
