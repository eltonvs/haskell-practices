-- Import Libs
import Graphics.UI.Gtk

main :: IO ()
main = do
    -- Function called by all Gtk2Hs applications
    initGUI

    -- Creates a new block and shows a window with your content (widgets)
    window <- windowNew
    widgetShowAll window

    -- The main application event loop
    mainGUI
