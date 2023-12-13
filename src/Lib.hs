module Lib where

import AppData
import Brick
import Brick.Widgets.Center
import Control.Exception (try)
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (chr, isSpace, toUpper)
import Data.List (dropWhileEnd, foldl1')
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector (Vector, (//))
import Foreign
import GHC.IO.IOMode
import Graphics.Vty
import Numeric (readHex, showHex)
import System.Directory
import qualified System.IO as IO
import System.IO.MMap
import Data.Word (Word8)

setStatus :: String -> EventM AppName AppState ()
setStatus s = do
  status .= s
  refreshStatus

refreshStatus :: EventM AppName AppState ()
refreshStatus =
  let vp = viewportScroll Status
   in hScrollToBeginning vp

funcMap :: M.Map String (EventM AppName AppState ())
funcMap =
  M.fromList
    [ ("Debug Status", debugStatus),
      ("Exit", exit),
      ("About", about),
      ("Debug Prompt", debugPrompt),
      ("Open ...", openPrompt),
      ("Save", saveFile),
      ("Save as ...", saveAsPrompt),
      ("Close", closeFile),
      ("Jump ...", jumpPrompt),
      ("Find ...", findPrompt)
    ]

debugStatus :: EventM AppName AppState ()
debugStatus = setStatus "Debug................................1................2........3....4..5.End"

exit :: EventM AppName AppState ()
exit = halt

about :: EventM AppName AppState ()
about =
  prompt
    .= Just
      ( MkPrompt
          { _pTitle = "About",
            _pBody = \_ _ -> hLimit width $ foldl1' (<=>) $ map (hCenter . str) aboutText,
            _pWidth = Just width,
            _pHeight = Just $ length aboutText,
            _pButtons = [("OK", exitPrompt)],
            _pButtonFocus = 0,
            _pExtraHandler = Nothing
          }
      )
  where
    width = maximum $ map length aboutText
    aboutText =
      [ "Haskell Hex Editor",
        "UCSD CSE 230 Fall 2023",
        "Authors"
      ]

exitPrompt :: EventM AppName AppState ()
exitPrompt = prompt .= Nothing

debugPrompt :: EventM AppName AppState ()
debugPrompt =
  prompt
    .= Just
      ( MkPrompt
          { _pTitle = "Debug",
            _pBody = \_ _ -> foldl1' (<=>) $ map str text,
            _pWidth = Nothing,
            _pHeight = Just 3,
            _pButtons =
              [ ("OK", exitPrompt),
                ("Long Button Text 2 ...... 3 .. 4  5 End", return ()),
                ("Test", return ())
              ],
            _pButtonFocus = 0,
            _pExtraHandler = Nothing
          }
      )
  where
    text =
      [ "Debug................................1................2........3....4..5.End",
        "Fill 1",
        "Fill 2",
        "Fill 3",
        "Fill 4",
        "Fill 5"
      ]

openPrompt :: EventM AppName AppState ()
openPrompt = do
  prompt
    .= Just
      ( MkPrompt
          { _pTitle = "Open File",
            _pBody = openBodyWidget,
            _pWidth = Nothing,
            _pHeight = Just 3,
            _pButtons =
              [ ("OK", openFile),
                ("Cancel", exitPrompt)
              ],
            _pButtonFocus = 0,
            _pExtraHandler = Just openHandler
          }
      )
  where
    openBodyWidget state (width, _) =
      Widget Fixed Fixed $
        render $
          hLimit width $
            vLimit 3 $
              let file =
                    if state ^. enterFile == ""
                      then " "
                      else state ^. enterFile
               in foldl1'
                    (<=>)
                    [ str "Enter file path:",
                      viewport OpenInput Horizontal $ str file,
                      str $ replicate width '-'
                    ]

saveAsPrompt :: EventM AppName AppState ()
saveAsPrompt = do
  prompt
    .= Just
      ( MkPrompt
          { _pTitle = "Save As...",
            _pBody = saveAsBodyWidget,
            _pWidth = Nothing,
            _pHeight = Just 3,
            _pButtons =
              [ ("OK", saveFileAs),
                ("Cancel", exitPrompt)
              ],
            _pButtonFocus = 0,
            _pExtraHandler = Just saveAsHandler
          }
      )
  where
    saveAsBodyWidget state (width, _) =
      Widget Fixed Fixed $
        render $
          hLimit width $
            vLimit 3 $
              let file =
                    if state ^. newFile == ""
                      then " "
                      else state ^. newFile
               in foldl1'
                    (<=>)
                    [ str "Enter file path:",
                      viewport OpenInput Horizontal $ str file,
                      str $ replicate width '-'
                    ]

findPrompt :: EventM AppName AppState ()
findPrompt = do
  prompt
    .= Just
      ( MkPrompt
          { _pTitle = "Find and Replace",
            _pBody = findBodyWidget,
            _pWidth = Nothing,
            _pHeight = Just 3,
            _pButtons =
              [ ("OK", findStringOff),
                ("Cancel", exitPrompt)
              ],
            _pButtonFocus = 0,
            _pExtraHandler = Just findAndReplaceHandler
          }
      )
  where
    findBodyWidget state (width, _) =
      Widget Fixed Fixed $
        render $
          hLimit width $
            vLimit 3 $
              let fStr =
                    if state ^. findString == ""
                      then " "
                      else state ^. findString
               in foldl1'
                    (<=>)
                    [ str "Enter the string to search:",
                      viewport OpenInput Horizontal $ str fStr,
                      str $ replicate width '-'
                    ]

openFile :: EventM AppName AppState ()
openFile = do
  path <- use enterFile
  isFile <- liftIO $ doesFileExist path
  if isFile
    then do
      rawPerm <- liftIO ((try $ getPermissions path) :: IO (Either IOError Permissions))
      case rawPerm of
        Left _ -> setStatus $ "Cannot check permission: " ++ path
        Right perm ->
          if readable perm
            then do
              rawSize <- liftIO ((try $ getFileSize path) :: IO (Either IOError Integer))
              case rawSize of
                Left _ -> setStatus $ "Cannot check size: " ++ path
                Right size ->
                  if size == 0
                    then setStatus $ "Empty file: " ++ path
                    else do
                      -- closeFile
                      if writable perm
                        then fileWrite .= True
                        else do
                          fileWrite .= False
                      -- setStatus $ "No write permission: " ++ path
                      fileSize .= size
                      fileOffset .= 0
                      fileRow .= 0
                      hexOffset .= 0
                      mmapOffset .= -1
                      perfCount .= 0
                      enterOffset .= ""
                      ext <- lookupExtent Editor
                      case ext of
                        Nothing -> setStatus "internal error"
                        Just (Extent _ _ (_, h)) ->
                          let h' = h - 1
                          in do
                                updateMmap h'
                                mmapFile <- use file
                                unless (isNothing mmapFile) $ do
                                  setStatus $
                                    "File opened: "
                                      ++ path
                                      ++ "; size: "
                                      ++ show size
                                      ++ "; mode: "
                                      ++ if writable perm
                                        then "ReadWrite"
                                        else "ReadOnly"
                                  mode .= Edit
        else setStatus $ "No read permission: " ++ path
    else setStatus $ "File not exist: " ++ path
  exitPrompt

updateMmap :: Int -> EventM AppName AppState ()
updateMmap h = do
  offset <- use fileOffset
  path <- use enterFile
  canWrite <- use fileWrite
  oldOffset <- use mmapOffset
  oldFile <- use file
  size <- use fileSize
  let rawSize = 48 * (h - 1)
      defaultSize = 12 * 1024
      bufferSize = max defaultSize ((defaultSize - rawSize) `mod` defaultSize + rawSize)
      splitSize = bufferSize `div` 3
      safeOffset = max 0 $ offset - (16 * (fromIntegral h - 1))
      newOffset = safeOffset - (safeOffset `mod` fromIntegral splitSize)
      safeBufferSize = min bufferSize $ fromInteger $ size - newOffset
      rawOffset = Just (fromInteger newOffset, safeBufferSize)
      perm =
        if canWrite
          then ReadWrite
          else ReadOnly
      oldBufferSize = case oldFile of
        Nothing -> (-1)
        Just (_, _, _, s) -> s
   in unless (newOffset == oldOffset && safeBufferSize == oldBufferSize) $ do
        closeMmap
        mmapFile <- liftIO ((try $ mmapFilePtr path perm rawOffset) :: IO (Either IOError (Ptr a, Int, Int, Int)))
        case mmapFile of
          Left e -> do
            closeFile
            setStatus $ "Error reading file: " ++ show e
          Right f -> do
            mmapOffset .= newOffset
            file .= Just f
            fillBuffer h

fillBuffer :: Int -> EventM AppName AppState ()
fillBuffer h = do
  mmapFile <- use file
  pCnt <- use perfCount
  size <- use fileSize
  mmapOff <- use mmapOffset
  modificationBuf <- use modificationBuffer
  let (ptr, _, o, _) = fromMaybe undefined mmapFile
      rawSize = 48 * (h - 1)
      defaultSize = 12 * 1024
      bufferSize = max defaultSize ((defaultSize - rawSize) `mod` defaultSize + rawSize)
      safeBufferSize = min bufferSize $ fromInteger $ size - mmapOff
   in do
        buffer <- liftIO ((try $ V.generateM safeBufferSize (peek . plusPtr (plusPtr ptr o))) :: IO (Either IOError (Vector Word8)))
        case buffer of
          Left _ -> do
            f <- use enterFile
            closeFile
            mode .= Cmd
            setStatus $ "Error reading buffer from file: " ++ f
          Right b -> do
            let mapped = map (\(off, updated) -> (fromInteger off - fromInteger mmapOff, updated)) (M.toList modificationBuf)
                filtered = filter (\(off, _) -> 0 <= off && off < safeBufferSize) mapped
            fileBuffer .= b // filtered
            perfCount .= pCnt + 1

saveFileAs :: EventM AppName AppState ()
saveFileAs = do
  path <- use enterFile
  newPath <- use newFile
  res <- liftIO ((try $ copyFile path newPath) :: IO (Either IOError ()))
  case res of
    Left e -> setStatus $ "Cannot copy to new file: " ++ show e
    Right _ -> do
      enterFile .= newPath
      saveFile
  newFile .= ""
  exitPrompt

saveFile :: EventM AppName AppState ()
saveFile = do
  path <- use enterFile
  modificationBuf <- use modificationBuffer
  rawHandle <- liftIO ((try $ IO.openFile path ReadWriteMode) :: IO (Either IOError IO.Handle))
  case rawHandle of
    Left _ -> setStatus $ "File not saved: Cannot open file: " ++ path
    Right handle -> do
      res <- mapM
        ( \(off, updated) ->
            liftIO ((try $ IO.hSeek handle IO.AbsoluteSeek off >> IO.hPutChar handle (chr $ fromIntegral updated)) :: IO (Either IOError ())))
        (M.toList modificationBuf)
      case sequence res of
        Left e -> setStatus $ "Save file error: " ++ show e
        Right _ -> do
          modificationBuffer .= M.empty
          setStatus $ "File saved: " ++ path
      _ <- liftIO ((try $ IO.hClose handle) :: IO (Either IOError ()))
      ext <- lookupExtent Editor
      case ext of
        Nothing -> setStatus "internal error"
        Just (Extent _ _ (_, h)) -> fillBuffer (h - 1)

findStringOff :: EventM AppName AppState ()
findStringOff = do
    currOff <- use fileOffset
    mOff <- use mmapOffset
    findFromStart currOff mOff True False

findFromStart :: Integer -> Integer -> Bool -> Bool -> EventM AppName AppState ()
findFromStart originOff originMOff isFst isFromBegin= do
  strToFind <- use findString
  curFileBuf <- use fileBuffer
  mmpOff <- use mmapOffset

  let startIdx = ( originOff) - ( mmpOff)
      vecToFind = stringToWord8Vector strToFind
      idx = if not isFst then isSubVector vecToFind curFileBuf 0 else isSubVector vecToFind curFileBuf (fromInteger (startIdx + 1))
  prevFileOff <- use fileOffset
  setStatus $ "current File Offset: " ++ (show prevFileOff) ++ "!current mmapOff: " ++ (show mmpOff) ++ "! isFST?" ++ (show isFst)

  case idx of
    Just i -> jumpOffset ( mmpOff + (fromIntegral i))
    Nothing -> do
      curmOff <- use mmapOffset
      jumpOffset (curmOff + fromIntegral ((V.length curFileBuf)))
      curFileOff <- use fileOffset
      if (curFileOff == prevFileOff) then
        if (isFromBegin) then jumpOffset originOff
        else do
          jumpOffset 0
          findFromStart originOff originMOff False True
      else
        if (isFromBegin && curFileOff > originMOff) then jumpOffset originOff
        else findFromStart originOff originMOff False isFromBegin

stringToWord8Vector :: String -> V.Vector Word8
stringToWord8Vector = V.fromList . map (fromIntegral . fromEnum)

isSubVector :: V.Vector Word8 -> V.Vector Word8 -> Int -> Maybe Int
isSubVector subVec vec checkFrom = checkSub 0
  where
    checkSub ids
      | ids + checkFrom + V.length subVec > V.length vec || ids + checkFrom >= V.length vec = Nothing -- Subvector won't fit anymore
      | V.slice (ids + checkFrom) (V.length subVec) vec == subVec = Just (ids + checkFrom) -- Subvector found
      | otherwise = checkSub (ids + 1) -- Move to the next position

closeMmap :: EventM AppName AppState ()
closeMmap = do
  mmapFile <- use file
  case mmapFile of
    Just (ptr, rs, _, _) -> do
      _ <- liftIO ((try $ munmapFilePtr ptr rs) :: IO (Either IOError ()))
      return ()
    Nothing -> return ()

clearBuffer :: EventM AppName AppState ()
clearBuffer = fileBuffer .= V.empty

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
  enterOffset .= ""
  enterFile .= ""
  setStatus "File closed"

openHandler :: BrickEvent AppName () -> EventM AppName AppState ()
openHandler event = case event of
  VtyEvent (EvKey key modifier) -> case modifier of
    [] -> case key of
      KChar c -> do
        prevFile <- use enterFile
        enterFile .= prevFile ++ [c]
        scroll
      KBS -> do
        prevFile <- use enterFile
        if prevFile == ""
          then return ()
          else do
            enterFile .= init prevFile
            scroll
      KDel -> enterFile .= ""
      _ -> return ()
    _ -> return ()
  VtyEvent EvResize {} -> scroll
  _ -> return ()
  where
    scroll =
      let vp = viewportScroll OpenInput
       in hScrollToEnd vp

saveAsHandler :: BrickEvent AppName () -> EventM AppName AppState ()
saveAsHandler event = case event of
  VtyEvent (EvKey key modifier) -> case modifier of
    [] -> case key of
      KChar c -> do
        prevFile <- use newFile
        newFile .= prevFile ++ [c]
        scroll
      KBS -> do
        prevFile <- use newFile
        if prevFile == ""
          then return ()
          else do
            newFile .= init prevFile
            scroll
      KDel -> newFile .= ""
      _ -> return ()
    _ -> return ()
  VtyEvent EvResize {} -> scroll
  _ -> return ()
  where
    scroll =
      let vp = viewportScroll OpenInput
       in hScrollToEnd vp

findAndReplaceHandler :: BrickEvent AppName () -> EventM AppName AppState ()
findAndReplaceHandler event = case event of
  VtyEvent (EvKey key modifier) -> case modifier of
    [] -> case key of
      KChar c -> do
        toFind <- use findString
        findString .= toFind ++ [c]
        scroll
      KBS -> do
        toFind <- use findString
        if toFind == ""
          then return ()
          else do
            findString .= init toFind
            scroll
      KDel -> findString .= ""
      _ -> return ()
    _ -> return ()
  VtyEvent EvResize {} -> scroll
  _ -> return ()
  where
    scroll =
      let vp = viewportScroll OpenInput
       in hScrollToEnd vp

jumpOffset :: Integer -> EventM AppName AppState ()
jumpOffset offset = do
  ext <- lookupExtent Editor
  size <- use fileSize
  minRow <- use fileRow
  case ext of
    Nothing -> setStatus "internal error"
    Just (Extent _ _ (_, h)) ->
      let newOffset = min (size - 1) $ max 0 offset
          rowNum = newOffset `div` 16
          maxRow = minRow + fromIntegral h - 2
       in do
            fileOffset .= newOffset
            if maxRow < minRow
              then return ()
              else do
                when (rowNum < minRow) $ do
                  fileRow .= rowNum
                  updateMmap (h - 1)
                when (rowNum > maxRow) $ do
                  fileRow .= rowNum - maxRow + minRow
                  updateMmap (h - 1)

jumpPrompt :: EventM AppName AppState ()
jumpPrompt = do
  prompt
    .= Just
      ( MkPrompt
          { _pTitle = "Jump",
            _pBody = jumpWidget,
            _pWidth = Nothing,
            _pHeight = Just 3,
            _pButtons =
              [ ("OK", jumpEnterOffset),
                ("Cancel", exitPrompt)
              ],
            _pButtonFocus = 0,
            _pExtraHandler = Just jumpHandler
          }
      )
  where
    jumpWidget state (width, _) =
      Widget Fixed Fixed $
        render $
          hLimit width $
            vLimit 3 $
              let offset =
                    if state ^. enterOffset == ""
                      then " "
                      else state ^. enterOffset
               in foldl1'
                    (<=>)
                    [ str "Enter offset in hex:",
                      viewport JumpInput Horizontal $ str offset,
                      str $ replicate width '-'
                    ]

trimStr :: String -> String
trimStr = dropWhileEnd isSpace . dropWhile isSpace

jumpEnterOffset :: EventM AppName AppState ()
jumpEnterOffset = do
  mmapFile <- use file
  case mmapFile of
    Nothing -> setStatus "Cannot jump: no file opened"
    Just _ -> do
      off <- use enterOffset
      let trimOff = trimStr off
          result = readHex trimOff
       in case result of
            [] -> setStatus $ "Cannot parse hex input: " ++ trimOff
            _ ->
              let (offset, rest) = head result
               in if rest == ""
                    then do
                      jumpOffset offset
                      newOff <- use fileOffset
                      setStatus $ "Jumped to offset: " ++ map toUpper (showHex newOff "")
                      mode .= Edit
                    else setStatus $ "Cannot parse partial hex input: " ++ rest
  exitPrompt

jumpHandler :: BrickEvent AppName () -> EventM AppName AppState ()
jumpHandler event = case event of
  VtyEvent (EvKey key modifier) -> case modifier of
    [] -> case key of
      KChar c -> do
        prevOffset <- use enterOffset
        enterOffset .= prevOffset ++ [c]
        scroll
      KBS -> do
        prevOffset <- use enterOffset
        if prevOffset == ""
          then return ()
          else do
            enterOffset .= init prevOffset
            scroll
      KDel -> enterOffset .= ""
      _ -> return ()
    _ -> return ()
  VtyEvent EvResize {} -> scroll
  _ -> return ()
  where
    scroll =
      let vp = viewportScroll JumpInput
       in hScrollToEnd vp
