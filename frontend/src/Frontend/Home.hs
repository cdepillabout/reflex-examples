{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Frontend.Home (home) where

import Common.Route (FrontendRoute(FrontendRoute_Examples), exampleDescription, exampleTitle, sectionHomepage)
import Control.Monad (forM_)
import Data.Dependent.Sum (DSum)
import Data.Functor.Identity (Identity)
import Data.Universe (universe)
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, pattern (:/), routeLink)
import Reflex.Dom (DomBuilder, (=:), el, elAttr, elClass, text)

home
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
home = do
  elClass "p" "class" $ do
    text "Reflex is a fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."
  elClass "p" "class" $ do
    text "The following are some examples of using Reflex along with "
    let obSrc = "https://github.com/obsidiansystems/obelisk"
    elAttr "a" ("href" =: obSrc <> "target" =: "_blank") $ text "Obelisk."
  elClass "p" "class" $ do
    text "To run these examples on your machine, install Obelisk, clone this repo and run 'ob run' command."
  examplesList

examplesList
  :: ( DomBuilder t m
     -- , SetRoute t (R FrontendRoute) m
     -- , RouteToUrl (R FrontendRoute) m
     , SetRoute t (DSum FrontendRoute Identity) m
     , RouteToUrl (DSum FrontendRoute Identity) m
     )
  => m ()
examplesList = el "ul" $ do
  -- Iterate over all the top-level routes
  forM_ universe $ \section -> el "li" $ do
    el "h4" $ routeLink (FrontendRoute_Examples :/ (Just $ sectionHomepage section)) $
      text $ exampleTitle section
    el "p" $ text (exampleDescription section)
