
module Main where

import Frontend (frontend)
import Common.Route (backendRouteEncoder)
import Obelisk.Frontend (runFrontend)
import Obelisk.Route.Frontend (checkEncoder)
import Reflex.Dom (run)

-- checkEncoder :: (Applicative check', Functor check)
--   => Encoder check parse decoded encoded
--   -> check (Encoder check' parse decoded encoded)

-- backendRouteEncoder
--   :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName

-- runFrontend
--   :: forall backendRoute route
--    . Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName
--   -> Frontend (R route)
--   -> JSM ()

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder backendRouteEncoder
  run $ runFrontend validFullEncoder frontend
