{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module FriedemMain where

import           Control.Concurrent
import           Control.Concurrent.STM
import           FSM
import           GUI
import           Network.Socket
import           System.IO
import           Control.Monad
import GI.Gtk (Entry, TextBuffer, TextView, textViewGetBuffer, entryGetText, entrySetText, textBufferGetEndIter, textBufferInsert)
import qualified Data.Text as T 
import qualified Data.Text.IO as T

friedemMain :: TChan ClientEvent -> GUI -> FSM ClientState ClientEvent
friedemMain evtQ gui@GUI{..} Disconnected SendButtonPressed = return Disconnected
friedemMain evtQ gui@GUI{..} Disconnected ClientButtonPressed = do
  --TODO: Somehow prompt for IP addr, using constant for now
  let ipAddr = "127.0.0.1"
  addr <- inet_addr ipAddr
  sock <- socket AF_INET Stream 0
  connect sock (SockAddrInet 4242 addr)
  --TODO: Should probably check if the connection is live
  hdl <- socketToHandle sock ReadWriteMode
  buffer <- textViewGetBuffer msgView
  forkIO $ recieveThread hdl buffer
  return $ ConnectedClient hdl

friedemMain evtQ gui@GUI{..} Disconnected (ChatRequestRecv hdl) = do
  buffer <- textViewGetBuffer msgView
  forkIO $ recieveThread hdl buffer
  return $ ConnectedHost hdl

friedemMain evtQ gui@GUI{..} (ConnectedHost hdl) SendButtonPressed = do
  sendMsg hdl msgEntry msgView
  return $ ConnectedHost hdl

friedemMain evtQ gui@GUI{..} (ConnectedClient hdl) SendButtonPressed = do
  sendMsg hdl msgEntry msgView
  return $ ConnectedHost hdl

recieveThread :: Handle -> TextBuffer -> IO ()
recieveThread hdl buffer = forever $ T.hGetLine hdl >>= appendToMessageBuffer buffer . prependSrc
  where
    prependSrc :: T.Text -> T.Text
    prependSrc msg = T.append "In: " msg

sendMsg :: Handle -> Entry -> TextView -> IO ()
sendMsg hdl entry view = do
  msg <- entryGetText entry
  entrySetText entry ""
  T.hPutStrLn hdl msg
  buffer <- textViewGetBuffer view
  appendToMessageBuffer buffer (T.append "Sent: " msg)

hostNetworkThread :: TChan ClientEvent -> IO ()
hostNetworkThread evtQ = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  (sock', addr) <- accept sock
  --TODO: Prompt user to accept connection
  putStrLn "Connection Accepted!"
  hdl <- socketToHandle sock' ReadWriteMode
  hSetBuffering hdl NoBuffering

  let sendEvent = atomically . writeTChan evtQ
  sendEvent $ ChatRequestRecv hdl

appendToMessageBuffer :: TextBuffer -> T.Text -> IO ()
appendToMessageBuffer buffer msg = do
  endItr <- textBufferGetEndIter buffer
  let msg' = T.append msg "\n"
  textBufferInsert buffer endItr msg' (fromIntegral (T.length msg'))
