{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk (widgetShowAll, mainQuit, onWidgetDestroy, onButtonClicked, onMenuItemActivate)
import           Control.Concurrent.STM
import           Control.Concurrent
import GUI
import FSM
import FriedemMain

main :: IO ()
main = do
  Gtk.init Nothing

  evtQ <- atomically newTChan
  gui@GUI{..} <- loadGlade "GUI.glade"

  forkIO $ hostNetworkThread evtQ
  forkIO $ runFsm (friedemMain evtQ gui) Disconnected evtQ

  onWidgetDestroy mainWin mainQuit

  let sendEvent = atomically . writeTChan evtQ
  onMenuItemActivate hostBtn $ sendEvent HostButtonPressed
  onMenuItemActivate  clientBtn $ sendEvent ClientButtonPressed
  onButtonClicked sendBtn $ sendEvent SendButtonPressed

  -- Display the window
  widgetShowAll mainWin
  Gtk.main

