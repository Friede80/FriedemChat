{-# LANGUAGE OverloadedStrings #-}
module GUI where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk
       (widgetShowAll, mainQuit, onWidgetDestroy, onButtonClicked, Button(..),
        Window(..), Entry(..), TextView(..), MenuItem(..), builderGetObject, builderAddFromFile, builderNew)

data GUI =
  GUI { mainWin :: Window
      , sendBtn :: Button
      , msgEntry :: Entry
      --, convList :: ListBox
      , msgView :: TextView
      , hostBtn :: MenuItem
      , clientBtn :: MenuItem }

loadGlade :: Text -> IO GUI
loadGlade gladeFP = do
  -- Create the builder, and load the UI file
  builder <- builderNew
  builderAddFromFile builder gladeFP

  -- Retrieve some objects from the UI
  mw <- builderGetObject builder "MainWindow" >>= unsafeCastTo Window . fromJust
  sb <- builderGetObject builder "SendBtn" >>= unsafeCastTo Button . fromJust
  me <- builderGetObject builder "MsgEntry" >>= unsafeCastTo Entry . fromJust
  mv <- builderGetObject builder "MsgView" >>= unsafeCastTo TextView . fromJust
  hb <- builderGetObject builder "HostBtn" >>= unsafeCastTo MenuItem . fromJust
  cb <- builderGetObject builder "ClientBtn" >>= unsafeCastTo MenuItem . fromJust

  return $ GUI mw sb me  mv hb cb
