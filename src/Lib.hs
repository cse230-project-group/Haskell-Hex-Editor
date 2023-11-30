module Lib where

import AppData
import Brick
import Brick.Widgets.Center
import Control.Lens
import Data.List (foldl1')

import qualified Data.Map as M

setStatus :: String -> EventM AppName AppState ()
setStatus s = do
    status .= s
    refreshStatus

refreshStatus :: EventM AppName AppState ()
refreshStatus = let vp = viewportScroll Status in
    hScrollToBeginning vp

funcMap :: M.Map String (EventM AppName AppState ())
funcMap = M.fromList [ ("Debug Status", debugStatus)
                     , ("Exit", exit)
                     , ("About", about)
                     , ("Debug Prompt", debugPrompt)
                     ]

debugStatus :: EventM AppName AppState ()
debugStatus = setStatus "Debug................................1................2........3....4..5.End"

exit :: EventM AppName AppState ()
exit = halt

about :: EventM AppName AppState ()
about = prompt .= Just (MkPrompt
    { _pTitle = "About"
    , _pBody = hLimit width $ foldl1' (<=>) $ map (hCenter . str) aboutText
    , _pWidth = Just width
    , _pHeight = Just $ length aboutText
    , _pButtons = [ ("OK", exitPrompt) ]
    , _pButtonFocus = 0
    , _pExtraHandler = Nothing
    })
    where
        width = maximum $ map length aboutText
        aboutText = [ "Haskell Hex Editor"
                    , "UCSD CSE 230 Fall 2023"
                    , "Authors"
                    ]

exitPrompt :: EventM AppName AppState ()
exitPrompt = prompt .= Nothing

debugPrompt :: EventM AppName AppState ()
debugPrompt = prompt .= Just (MkPrompt
    { _pTitle = "Debug"
    , _pBody = foldl1' (<=>) $ map str text
    , _pWidth = Nothing
    , _pHeight = Just 3
    , _pButtons = [ ("OK", exitPrompt)
                  , ("Long Button Text 2 ...... 3 .. 4  5 End", return ())
                  , ("Test", return ())
                  ]
    , _pButtonFocus = 0
    , _pExtraHandler = Nothing
    })
    where
        text = [ "Debug................................1................2........3....4..5.End"
               , "Fill 1"
               , "Fill 2"
               , "Fill 3"
               , "Fill 4"
               , "Fill 5"
               ]
