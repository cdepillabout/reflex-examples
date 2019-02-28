
module Main where

import Frontend (frontend)
import Common.Route (backendRouteEncoder)
import Obelisk.Frontend (runFrontend)
import Obelisk.Route.Frontend (checkEncoder)
import Reflex.Dom (run)

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder backendRouteEncoder
  run $ runFrontend validFullEncoder frontend
