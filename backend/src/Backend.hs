{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route (BackendRoute(BackendRoute_Missing, BackendRoute_WebSocketChat), FrontendRoute, backendRouteEncoder)
import Obelisk.Backend (Backend(Backend, _backend_run, _backend_routeEncoder))
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity (Identity(Identity))
import Control.Concurrent (newMVar)
import Network.WebSockets.Snap (runWebSocketsSnap)

import qualified Backend.Examples.WebSocketChat.Server as WebSocketChat

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      webSocketChatState <- newMVar WebSocketChat.newServerState
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_WebSocketChat :=> Identity () -> do
          runWebSocketsSnap (WebSocketChat.application webSocketChatState)

  , _backend_routeEncoder = backendRouteEncoder
  }
