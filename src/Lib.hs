module Lib where

import AppData
import Brick
import Brick.Widgets.Center
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (foldl1')
import Data.Maybe
import Data.Vector (generateM, empty)
import Foreign
import Graphics.Vty
import System.Directory
import System.IO.MMap

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
                     , ("Open ...", openPrompt)
                     , ("Close", closeFile)
                     ]

debugStatus :: EventM AppName AppState ()
debugStatus = setStatus "Debug................................1................2........3....4..5.End"

exit :: EventM AppName AppState ()
exit = halt

about :: EventM AppName AppState ()
about = prompt .= Just (MkPrompt
    { _pTitle = "About"
    , _pBody = \_ -> hLimit width $ foldl1' (<=>) $ map (hCenter . str) aboutText
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
    , _pBody = \_ -> foldl1' (<=>) $ map str text
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

openPrompt :: EventM AppName AppState ()
openPrompt = do
    prompt .= Just (MkPrompt
        { _pTitle = "Open File"
        , _pBody = openBodyWidget
        , _pWidth = Just width
        , _pHeight = Just 3
        , _pButtons = [ ("OK", openFile)
                      , ("Cancel", exitPrompt)
                      ]
        , _pButtonFocus = 0
        , _pExtraHandler = Just openHandler
        })
    where
        width = 40
        openBodyWidget state = Widget Fixed Fixed $ render $ hLimit width $ vLimit 3 $
            let file = if state ^. enterFile == ""
                then
                    " "
                else
                    state ^. enterFile
                in
                    foldl1' (<=>) [ str "Enter file path:"
                                  , viewport OpenInput Horizontal $ str file
                                  , str $ replicate width '-'
                                  ]

openFile :: EventM AppName AppState ()
openFile = do
    path <- use enterFile
    isFile <- liftIO $ doesFileExist path
    if isFile
        then do
            perm <- liftIO $ getPermissions path
            if readable perm
                then do
                    closeFile
                    if writable perm
                        then
                            fileWrite .= True
                        else do
                            fileWrite .= False
                            --setStatus $ "No write permission: " ++ path
                    size <- liftIO $ getFileSize path
                    fileSize .= size
                    fileOffset .= 0
                    fileRow .= 0
                    hexOffset .= 0
                    mmapOffset .= -1
                    perfCount .= 0
                    ext <- lookupExtent Editor
                    case ext of
                        Nothing -> setStatus "internal error"
                        Just (Extent _ _ (_, h)) -> let h' = h - 1 in updateMmap h'
                    setStatus $ "File opened: " ++ path ++ "; size: " ++ show size ++ "; mode: " ++
                        if writable perm
                            then
                                "ReadWrite"
                            else
                                "ReadOnly"
                    mode .= Edit
                    exitPrompt
                else do
                    setStatus $ "No read permission: " ++ path
                    exitPrompt
        else do
            setStatus $ "File not exist: " ++ path
            exitPrompt

updateMmap :: Int -> EventM AppName AppState ()
updateMmap h = do
    offset <- use fileOffset
    path <- use enterFile
    canWrite <- use fileWrite
    oldOffset <- use mmapOffset
    oldFile <- use file
    let rawSize = 48 * (h - 1)
        bufferSize = max (12 * 1024) ((4096 - rawSize) `mod` 4096 + rawSize)
        splitSize = bufferSize `div` 3
        safeOffset = max 0 $ offset - (16 * (fromIntegral h - 1))
        newOffset = safeOffset - (safeOffset `mod` fromIntegral splitSize)
        rawOffset = Just (fromInteger newOffset, bufferSize)
        perm = if canWrite
            then
                ReadWrite
            else
                ReadOnly
        oldBufferSize = case oldFile of
            Nothing -> (-1)
            Just (_, _, _, s) -> s
        in
            unless (newOffset == oldOffset && bufferSize == oldBufferSize) $ do
                closeMmap
                mmapFile <- liftIO $ mmapFilePtr path perm rawOffset
                mmapOffset .= newOffset
                file .= Just mmapFile
                fillBuffer h
                --setStatus $ path ++ ", " ++ show rawOffset

fillBuffer :: Int -> EventM AppName AppState ()
fillBuffer h = do
    mmapFile <- use file
    pCnt <- use perfCount
    let (ptr, _, o, _) = fromMaybe undefined mmapFile
        rawSize = 48 * (h - 1)
        bufferSize = max (12 * 1024) ((4096 - rawSize) `mod` 4096 + rawSize)
        in do
            buffer <- liftIO $ generateM bufferSize (peek . plusPtr (plusPtr ptr o))
            fileBuffer .= buffer
            perfCount .= pCnt + 1
            --setStatus $ "loaded buffer " ++ show (pCnt + 1)

closeMmap :: EventM AppName AppState ()
closeMmap = do
    mmapFile <- use file
    case mmapFile of
        Just (ptr, rs, _, _) -> liftIO $ munmapFilePtr ptr rs
        Nothing -> return ()

clearBuffer :: EventM AppName AppState ()
clearBuffer = fileBuffer .= empty

closeFile :: EventM AppName AppState ()
closeFile = do
    clearBuffer
    closeMmap
    file .= Nothing
    fileWrite .= False
    fileSize .= 0
    fileOffset .= 0
    fileRow .= 0
    hexOffset .= 0
    mmapOffset .= -1
    perfCount .= 0
    setStatus "File closed"

openHandler :: BrickEvent AppName () -> EventM AppName AppState ()
openHandler (VtyEvent (EvKey key modifier)) = case modifier of
    [] -> case key of
        KChar c -> do
            prevFile <- use enterFile
            enterFile .= prevFile ++ [c]
            scroll
        KBS -> do
            prevFile <- use enterFile
            if prevFile == ""
                then
                    return ()
                else do
                    enterFile .= init prevFile
                    scroll
        _ -> return ()
    _ -> return ()
    where
        scroll = let vp = viewportScroll OpenInput in
            hScrollToEnd vp
openHandler _ = return ()
