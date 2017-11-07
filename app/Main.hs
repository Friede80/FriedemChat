{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad
import Control.Monad.IO.Class
import Network.Socket
import System.IO
import Control.Concurrent


data GUI =
  GUI { mainWin :: Window
      , sendBtn :: Button
      , msgEntry :: Entry
      --, convList :: ListBox
      , msgView :: TextView
      , hostBtn :: MenuItem
      , clientBtn :: MenuItem }

main :: IO ()
main = do
  initGUI
  gui <- loadGlade "GUI.glade"
  initialCallbacks gui
  widgetShowAll $ mainWin gui
  mainGUI

initHostConnection :: GUI -> IO ()
initHostConnection GUI{..} = do
  --msgDialog <- messageDialogNew Nothing [] MessageInfo ButtonsOk "Awaiting Connection"
  --dialogRun msgDialog

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  (sock', _) <- accept sock
  hdl <- socketToHandle sock' ReadWriteMode
  hSetBuffering hdl NoBuffering
  sendBtn `on` buttonActivated  $ sendMsg hdl msgEntry msgView
  buffer <- textViewGetBuffer msgView
  forkIO $ recieveThread hdl buffer
  return ()

initClientConnection :: GUI -> String -> IO ()
initClientConnection GUI{..} ipAddr = do
  sock <- socket AF_INET Stream 0
  addr <- inet_addr ipAddr
  connect sock (SockAddrInet 4242 addr)
  hdl <- socketToHandle sock ReadWriteMode
  sendBtn `on` buttonActivated  $ sendMsg hdl msgEntry msgView
  buffer <- textViewGetBuffer msgView
  forkIO $ recieveThread hdl buffer
  return ()

appendToMessageBuffer :: TextBuffer -> String -> IO ()
appendToMessageBuffer buffer msg = do
  endItr <- textBufferGetEndIter buffer
  textBufferInsert buffer endItr (msg ++ "\n")

recieveThread :: Handle -> TextBuffer -> IO ()
recieveThread hdl buffer = forever $ hGetLine hdl >>= appendToMessageBuffer buffer . prependSrc
  where
    prependSrc :: String -> String
    prependSrc msg = "In: " ++ msg

initialCallbacks :: GUI -> IO ()
initialCallbacks gui@GUI{..} = do
  mainWin `on` deleteEvent $ quitApp
  hostBtn `on` menuItemActivated  $ initHostConnection gui
  clientBtn `on` menuItemActivated  $ initClientConnection gui "127.0.0.1"
  return ()

sendMsg :: Handle -> Entry -> TextView -> IO ()
sendMsg sockHdl entry view = do
  msg <- entryGetText entry
  entrySetText entry ""
  hPutStrLn sockHdl msg
  buffer <- textViewGetBuffer view
  appendToMessageBuffer buffer ("Sent: " ++ msg)

loadGlade :: String -> IO GUI
loadGlade gladeFP = do
  builder <- builderNew
  builderAddFromFile builder gladeFP
  mw <- builderGetObject builder castToWindow "MainWindow"
  sb <- builderGetObject builder castToButton "SendBtn"
  me <- builderGetObject builder castToEntry "MsgEntry"
  --cl <- builderGetObject builder castToListBox "ConvList"
  mv <- builderGetObject builder castToTextView "MsgView"
  hb <- builderGetObject builder castToMenuItem "HostBtn"
  cb <- builderGetObject builder castToMenuItem "ClientBtn"

  return $ GUI mw sb me  mv hb cb


quitApp :: EventM a Bool
quitApp = liftIO mainQuit >> return False
