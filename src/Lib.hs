module Lib where

import AppData
import Brick
import Control.Lens
import qualified Data.Map as M

funcMap :: M.Map String (EventM AppName AppState ())
funcMap = M.fromList [ ("Debug Status", debugStatus)
                     , ("Exit", exit)
                     ]

debugStatus :: EventM AppName AppState ()
debugStatus = status .= "Debug"

exit :: EventM AppName AppState ()
exit = halt
