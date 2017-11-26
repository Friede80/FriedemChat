module FSM where

import Network.Socket
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.IO

data ClientState
  = Disconnected
  | RequestReceived HostAddress
  | AwaitingConnection HostAddress
  | ConnectedHost Handle
  | ConnectedClient Handle

data ClientEvent
  = ChatRequestRecv Handle
  | ChatRequestSend HostAddress
  | MessageRecv String
  | MessageSend String
  | TerminateConn
  | SendButtonPressed
  | ClientButtonPressed
  | HostButtonPressed

type FSM s e = s -> e -> IO s

runFsm :: FSM s e -> s -> TChan e -> IO ()
runFsm fsm s es = void $ runFsm' fsm s es
  where
    runFsm' :: FSM s e -> s -> TChan e -> IO s
    runFsm' fsm s es = do
      e <- atomically $ readTChan es
      s' <- fsm s e
      runFsm' fsm s' es
