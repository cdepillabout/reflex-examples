{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Sum (Sum(InL, InR))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Dependent.Sum (DSum (..))

import Obelisk.Route
  ( Encoder
  , ObeliskRoute
  , PageName
  , R
  , SegmentResult(PathEnd, PathSegment)
  , pattern (:/)
  , handleEncoder
  , maybeEncoder
  , obeliskRouteSegment
  , pathComponentEncoder
  , unitEncoder
  )
import Obelisk.Route.TH (deriveRouteComponent)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_WebSocketChat :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Examples :: FrontendRoute (Maybe (R Example))
deriving instance Show (FrontendRoute a)

data Example :: * -> * where
  Example_BasicToDo :: Example ()
  Example_DragAndDrop :: Example ()
  Example_FileReader :: Example ()
  Example_ScreenKeyboard :: Example ()
  Example_NasaPod :: Example ()
  Example_PegSolitaire :: Example ()
  Example_TicTacToe :: Example ()
  Example_DisplayGameUpdates :: Example ()
  Example_ECharts :: Example ()
  Example_WebSocketEcho :: Example ()
  Example_WebSocketChat :: Example ()
deriving instance Show (Example a)

-- This is the type of route encoder/decoders. It is parameterised over two
-- monads: Firstly, the monad used to check the validity of the encoder (i.e.
-- that it is total), secondly the monad used for parsing during the decode
-- phase. The following two parameters are respectively the type of decoded
-- data, and the encoded type.
-- newtype Encoder check parse decoded encoded =
--   Encoder { unEncoder :: check (EncoderImpl parse decoded encoded) }

-- data EncoderImpl parse decoded encoded = EncoderImpl
--   { _encoderImpl_decode :: !(encoded -> parse decoded) -- Can fail; can lose information; must always succeed on outputs of `_encoderImpl_encode` and result in the original value
--   , _encoderImpl_encode :: !(decoded -> encoded) -- Must be injective
--   }

-- data EncoderImpl (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName = EncoderImpl
--   { _encoderImpl_decode :: !(PageName -> (R (Sum BackendRoute (ObeliskRoute FrontendRoute))))
--   , _encoderImpl_encode :: !((R (Sum BackendRoute (ObeliskRoute FrontendRoute))) -> PageName)
--   }

-- A URL path and query string, in which trailing slashes don't matter in the
-- path and duplicate query parameters are not allowed. A final goal of
-- encoders using this library will frequently be to produce this.
-- type PageName = ([Text], Map Text (Maybe Text))

-- type R f = DSum f Identity

-- data EncoderImpl (DSum (Sum BackendRoute (ObeliskRoute FrontendRoute)) Identity) PageName = EncoderImpl
--   { _encoderImpl_decode :: !(PageName -> (DSum (Sum BackendRoute (ObeliskRoute FrontendRoute)) Identity))
--   , _encoderImpl_encode :: !((DSum (Sum BackendRoute (ObeliskRoute FrontendRoute)) Identity) -> PageName)
--   }

-- data DSum tag f = forall a. !(tag a) :=> f a

-- data ObeliskRoute :: (* -> *) -> * -> * where
--   -- We need to have the `f a` as an argument here, because otherwise we have no way to specifically check for overlap between us and the given encoder
--   ObeliskRoute_App :: f a -> ObeliskRoute f a
--   ObeliskRoute_Resource :: ResourceRoute a -> ObeliskRoute f a

-- | Handle an error in parsing, for example, in order to redirect to a 404 page.
-- handleEncoder :: (Functor check) => (e -> a) -> Encoder check (Either e) a b -> Encoder check Identity a b

-- | Encode a dependent sum of type `(R p)` into a PageName (i.e. the path and
-- query part of a URL) by using the supplied function to decide how to encode
-- the constructors of p using the SegmentResult type. It is important that the
-- number of values of type `(Some p)` be relatively small in order for
-- checking to complete quickly.
-- pathComponentEncoder
--   :: forall check parse p
--    . ( Universe (Some p)
--      , GShow p
--      , GCompare p
--      , MonadError Text check
--      , MonadError Text parse
--      )
--   => (forall a. p a -> SegmentResult check parse a)
--   -> Encoder check parse (R p) PageName

-- obeliskRouteSegment
--   :: forall check parse appRoute a.  (MonadError Text check, MonadError Text parse)
--   => ObeliskRoute appRoute a
--   -> (forall b. appRoute b -> SegmentResult check parse b)
--   -> SegmentResult check parse a

-- pattern (:/) :: f a -> a -> DSum f Identity
-- pattern a :/ b = a :=> Identity b

-- data SegmentResult check parse a
--   = PathEnd (Encoder check parse a (Map Text (Maybe Text)))
--   | PathSegment Text (Encoder check parse a PageName)

-- unitEncoder
--   :: (Applicative check, MonadError Text parse, Show r, Eq r)
--   => r -> Encoder check parse () r

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder noroute sumRoutes
  where
    noroute :: Text -> DSum (Sum BackendRoute (ObeliskRoute FrontendRoute)) Identity
    noroute _ = InL BackendRoute_Missing :=> Identity ()

    sumRoutes :: Encoder (Either Text) (Either Text) (DSum (Sum BackendRoute (ObeliskRoute FrontendRoute)) Identity) PageName
    sumRoutes = pathComponentEncoder pathRoutes

    pathRoutes
      :: Sum BackendRoute (ObeliskRoute FrontendRoute) a
      -> SegmentResult (Either Text) (Either Text) a
    pathRoutes (InL BackendRoute_Missing) =
      PathSegment "missing" $ unitEncoder mempty
    pathRoutes (InL BackendRoute_WebSocketChat) =
      PathSegment "websocketchat" $ unitEncoder mempty
    pathRoutes (InR obeliskRoute) = obeliskRouteSegment obeliskRoute obFrontRoute

    obFrontRoute :: FrontendRoute a -> SegmentResult (Either Text) (Either Text) a
    -- The encoder given to PathEnd determines how to parse query parameters,
    -- in this example, we have none, so we insist on it.
    obFrontRoute FrontendRoute_Home = PathEnd $ unitEncoder mempty
    obFrontRoute FrontendRoute_Examples =
      PathSegment "examples" $
        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ frontExampleRoutes

    frontExampleRoutes :: Example a -> SegmentResult (Either Text) (Either Text) a
    frontExampleRoutes Example_BasicToDo =
      PathSegment "basictodo" $ unitEncoder mempty
    frontExampleRoutes Example_DragAndDrop =
      PathSegment "draganddrop" $ unitEncoder mempty
    frontExampleRoutes Example_FileReader =
      PathSegment "filereader" $ unitEncoder mempty
    frontExampleRoutes Example_ScreenKeyboard =
      PathSegment "screenkeyboard" $ unitEncoder mempty
    frontExampleRoutes Example_NasaPod =
      PathSegment "nasapod" $ unitEncoder mempty
    frontExampleRoutes Example_PegSolitaire =
      PathSegment "pegsolitaire" $ unitEncoder mempty
    frontExampleRoutes Example_TicTacToe =
      PathSegment "tictactoe" $ unitEncoder mempty
    frontExampleRoutes Example_DisplayGameUpdates =
      PathSegment "displaygameupdates" $ unitEncoder mempty
    frontExampleRoutes Example_ECharts =
      PathSegment "echarts" $ unitEncoder mempty
    frontExampleRoutes Example_WebSocketEcho =
      PathSegment "websocketecho" $ unitEncoder mempty
    frontExampleRoutes Example_WebSocketChat =
      PathSegment "websocketchat" $ unitEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''Example
  ]


-- | Provide a human-readable name for a given section
exampleTitle :: Some Example -> Text
exampleTitle (Some.This sec) = case sec of
  Example_BasicToDo -> "Basic To Do List"
  Example_DragAndDrop -> "Drag n Drop"
  Example_FileReader -> "File Reader"
  Example_ScreenKeyboard -> "Onscreen Keyboard"
  Example_NasaPod -> "Nasa: Picture of the Day"
  Example_PegSolitaire -> "Peg Solitaire"
  Example_TicTacToe -> "Tic Tac Toe"
  Example_DisplayGameUpdates -> "Display Game Updates"
  Example_ECharts -> "ECharts Examples"
  Example_WebSocketEcho -> "WebSocket Echo"
  Example_WebSocketChat -> "WebSocket Chat"


-- | Provide a human-readable name for a route
routeTitle :: R FrontendRoute -> Text
routeTitle = \case
  (FrontendRoute_Home :=> _) -> "Examples"
  (FrontendRoute_Examples :=> Identity ex) -> case ex of
    (Nothing) -> "Examples"
    (Just (sec :=> _)) -> exampleTitle $ Some.This sec

-- | Given a section, provide its default route
sectionHomepage :: Some Example -> R Example
sectionHomepage (Some.This sec) = sec :/ case sec of
  Example_BasicToDo -> ()
  Example_DragAndDrop -> ()
  Example_FileReader -> ()
  Example_ScreenKeyboard -> ()
  Example_NasaPod -> ()
  Example_PegSolitaire -> ()
  Example_TicTacToe -> ()
  Example_DisplayGameUpdates -> ()
  Example_ECharts -> ()
  Example_WebSocketEcho -> ()
  Example_WebSocketChat -> ()

-- | Provide a human-readable description for a given section
exampleDescription :: Some Example -> Text
exampleDescription (Some.This sec) = case sec of
  Example_BasicToDo -> "A simple To-Do list app with user input handling and state management."
  Example_DragAndDrop ->
    "An example to demonstrate Drag and Drop functionality"
  Example_FileReader ->
    "Read a file on the client using FileReader"
  Example_ScreenKeyboard ->
    "Use an onscreen keyboard along with the normal user input"
  Example_NasaPod ->
    "Demonstrates XHR requests, by fetching Nasa' Astronomy Picture of the Day"
  Example_PegSolitaire ->
    "A simple client side game"
  Example_TicTacToe ->
    "A simple client side game"
  Example_DisplayGameUpdates ->
    "An example to demonstrate nested Dynamic values. A widget to show updates for a game."
  Example_ECharts ->
    "Usage of ECharts (external JS library) with GHCJS and Reflex"
  Example_WebSocketEcho ->
    "Demonstrates use of WebSocket by sending and receiving messages from websocket.org' echo API"
  Example_WebSocketChat ->
    "A simple chat server, this uses the common and backend packages to share data between client and server"

-- | Provide a human-readable description for a given route
routeDescription :: R FrontendRoute -> Text
routeDescription  = \case
  (FrontendRoute_Home :=> _) -> desc
  (FrontendRoute_Examples :=> Identity m) -> case m of
    (Nothing) -> desc
    (Just (ex :=> _)) -> exampleDescription $ Some.This ex
  where
    desc :: Text
    desc = "Welcome to Reflex Examples"

routeSourceCode :: R FrontendRoute -> Text
routeSourceCode = \case
  (FrontendRoute_Home :=> _) -> src
  (FrontendRoute_Examples :=> Identity m) -> case m of
    (Nothing) -> src
    (Just ex) -> exampleSourceCode ex
  where
    src :: Text
    src = "https://github.com/reflex-frp/reflex-examples"

exampleSourceCode :: R Example -> Text
exampleSourceCode (sec :=> _) = base <> path <> file
  where
    base = "https://github.com/reflex-frp/reflex-examples/blob/master/frontend/src/Frontend/Examples/"
    file = "/Main.hs"
    path = case sec of
      Example_BasicToDo -> "BasicToDo"
      Example_DragAndDrop -> "DragAndDrop"
      Example_FileReader -> "FileReader"
      Example_ScreenKeyboard -> "ScreenKeyboard"
      Example_NasaPod -> "NasaPod"
      Example_PegSolitaire -> "PegSolitaire"
      Example_TicTacToe -> "TicTacToe"
      Example_DisplayGameUpdates -> "DisplayGameUpdates"
      Example_ECharts -> "ECharts"
      Example_WebSocketEcho -> "WebSocketEcho"
      Example_WebSocketChat -> "WebSocketChat"
