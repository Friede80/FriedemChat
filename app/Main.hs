{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.UI.Gtk

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import GUI
import FSM
import FriedemMain

main :: IO ()
main = do
  evtQ <- atomically newTChan
  gui@GUI{..} <- loadGlade "GUI.glade"

  forkIO $ hostNetworkThread evtQ
  forkIO $ runFsm (friedemMain evtQ gui) Disconnected evtQ

  initGUI

  mainWin `on` deleteEvent $ quitApp

  let sendEvent = atomically . writeTChan evtQ
  hostBtn `on` menuItemActivated  $ sendEvent HostButtonPressed
  clientBtn `on` menuItemActivated  $ sendEvent ClientButtonPressed
  sendBtn `on` buttonActivated $ sendEvent SendButtonPressed

  widgetShowAll mainWin
  mainGUI
