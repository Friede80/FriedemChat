module GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Windows.Dialog
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

data IpPromptWindow =
  IpPromptWindow { ipPromptWin :: Window
                 , connBtn :: Button
                 , cancelBtn :: Button
                 , ipEntry :: Entry }

ipPromptWindow :: IO IpPromptWindow
ipPromptWindow = do
  builder <- builderNew
  builderAddFromFile builder "IPPrompt.glade"
  ipPrompt <- builderGetObject builder castToWindow "MainWindow"
  conB <- builderGetObject builder castToButton "ConnectBtn"
  canB <- builderGetObject builder castToButton "CancelBtn"
  ipAddrEntry <- builderGetObject builder castToEntry "IpAddrEntry"
  return $ IpPromptWindow ipPrompt conB canB ipAddrEntry
