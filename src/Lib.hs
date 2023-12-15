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
      ("Find Hex ...", findHexPrompt),
      ("Find ASCII ...", findPrompt)
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
openPrompt = prompt
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
              let file = state ^. enterFile
               in foldl1'
                    (<=>)
                    [ str "Enter file path:",
                      vLimit 1 $ viewport OpenInput Horizontal $ str file,
                      str $ replicate width '-'
                    ]

saveAsPrompt :: EventM AppName AppState ()
saveAsPrompt = prompt
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
              let file = state ^. newFile
               in foldl1'
                    (<=>)
                    [ str "Enter file path:",
                      vLimit 1 $ viewport SaveInput Horizontal $ str file,
                      str $ replicate width '-'
                    ]

findPrompt :: EventM AppName AppState ()
findPrompt = prompt
  .= Just
    ( MkPrompt
        { _pTitle = "Find and Replace (Ascii)",
          _pBody = findBodyWidget,
          _pWidth = Nothing,
          _pHeight = Just 6,
          _pButtons =
            [ ("Find", findStringOff),
              ("Replace", replaceHandler),
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
            vLimit 6 $
              let fStr = state ^. findString
                  rStr = state ^. replaceString
                in foldl1'
                    (<=>)
                    [ str "Find: (Press Ctrl + Any Key to switch between find and replace)",
                      vLimit 1 $ viewport FindInput Horizontal $ str fStr,
                      str $ replicate width '-',
                      str "Replace with:",
                      vLimit 1 $ viewport ReplaceInput Horizontal $ str rStr,
                      str $ replicate width '-'
                    ]

findHexPrompt :: EventM AppName AppState ()
findHexPrompt = prompt
  .= Just
    ( MkPrompt
        { _pTitle = "Find and Replace (Hex)",
          _pBody = findBodyWidget,
          _pWidth = Nothing,
          _pHeight = Just 6,
          _pButtons =
            [ ("Find", findHexOff),
              ("Replace", replaceHexHandler),
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
            vLimit 6 $
              let fStr = state ^. findString
                  rStr = state ^. replaceString
                in foldl1'
                    (<=>)
                    [ str "Find: (Press Ctrl + Any Key to switch between find and replace)",
                      vLimit 1 $ viewport FindInput Horizontal $ str fStr,
                      str $ replicate width '-',
                      str "Replace with:",
                      vLimit 1 $ viewport ReplaceInput Horizontal $ str rStr,
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
                      if writable perm
                        then fileWrite .= True
                        else fileWrite .= False
                      clearBuffer
                      closeMmap
                      clearModBuffer
                      file .= Nothing
                      fileSize .= size
                      currFile .= path
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
  path <- use currFile
  canWrite <- use fileWrite
  oldOffset <- use mmapOffset
  oldFile <- use file
  size <- use fileSize
  let rawSize = 48 * (h - 1)
      defaultSize = 12 * 1024
      bufferSize = max defaultSize ((defaultSize - rawSize) `mod` defaultSize + rawSize)
      splitSize = bufferSize `div` 3
      safeOffset = max 0 $ offset - fromIntegral splitSize
      newOffset = safeOffset - safeOffset `mod` fromIntegral splitSize
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
            fillBuffer h oldOffset oldBufferSize

fillBuffer :: Int -> Integer -> Int -> EventM AppName AppState ()
fillBuffer h oldOff oldSz = do
  mmapFile <- use file
  pCnt <- use perfCount
  size <- use fileSize
  mmapOff <- use mmapOffset
  modificationBuf <- use modificationBuffer
  oldBuf <- use fileBuffer
  let (ptr, _, o, _) = fromMaybe undefined mmapFile
      rawSize = 48 * (h - 1)
      defaultSize = 12 * 1024
      bufferSize = max defaultSize ((defaultSize - rawSize) `mod` defaultSize + rawSize)
      safeBufferSize = min bufferSize $ fromInteger $ size - mmapOff
      deltaOff = mmapOff - oldOff
   in do
        buffer <- liftIO ((try $ V.generateM safeBufferSize (\i ->
          let oldId = fromIntegral i + deltaOff in
            if oldId >= 0 && oldId < fromIntegral oldSz
              then
                return $ oldBuf V.! fromInteger oldId
              else
                peek $ plusPtr (plusPtr ptr o) i)) :: IO (Either IOError (Vector Word8)))
        case buffer of
          Left _ -> do
            f <- use currFile
            closeFile
            mode .= Cmd
            setStatus $ "Error reading buffer from file: " ++ f
          Right b -> do
            let mapped = map (\(off, updated) -> (fromInteger (off - mmapOff), updated)) (M.toList modificationBuf)
                filtered = filter (\(off, _) -> 0 <= off && off < safeBufferSize) mapped
            fileBuffer .= b // filtered
            perfCount .= pCnt + 1
            --setStatus $ "fillBuffer called times: " ++ show (pCnt + 1)

saveFileAs :: EventM AppName AppState ()
saveFileAs = do
  path <- use currFile
  newPath <- use newFile
  res <- liftIO ((try $ copyFile path newPath) :: IO (Either IOError ()))
  case res of
    Left e -> setStatus $ "Cannot copy to new file: " ++ show e
    Right _ -> do
      currFile .= newPath
      saveFile
  newFile .= ""
  exitPrompt

saveFile :: EventM AppName AppState ()
saveFile = do
  path <- use currFile
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
      return ()

findStringOff :: EventM AppName AppState ()
findStringOff = do
  mmapFile <- use file
  case mmapFile of
    Nothing -> setStatus "Cannot find Ascii: no file opened"
    Just _ -> do
      findStr <- use findString
      findBuffer .= stringToWord8Vector findStr
      currOff <- use fileOffset
      mOff <- use mmapOffset
      findFromStart currOff mOff True False

findHexOff :: EventM AppName AppState ()
findHexOff = do
  mmapFile <- use file
  case mmapFile of
    Nothing -> setStatus "Cannot find Hex: no file opened"
    Just _ -> do
      str <- use findString
      let trimOff = trimStr str
          result = readHex trimOff
       in case result of
        [] -> setStatus $ "Cannot parse hex input: " ++ trimOff
        _ ->
          let (offset, rest) = head result
            in if rest == "" then
              if length trimOff `mod` 2 == 1
                  then setStatus $ "Cannot parse hex input: " ++ trimOff
                  else do
                    findBuffer .= convertHex trimOff
                    currOff <- use fileOffset
                    mOff <- use mmapOffset
                    findFromStart currOff mOff True False
              else setStatus $ "Cannot parse partial hex input: " ++ rest

convertHex :: String -> Vector Word8
convertHex [] = V.empty
convertHex [_] = undefined
convertHex (x:y:xs) = V.singleton (fst $ head $ readHex $ x : [y]) V.++ convertHex xs

replaceHandler :: EventM AppName AppState ()
replaceHandler = do
  mmapFile <- use file
  case mmapFile of
    Nothing -> setStatus "Cannot replace Ascii: no file opened"
    Just _ -> do
      findStringOff
      curFileOff <- use fileOffset
      curMOff <- use mmapOffset
      findbuf <- use findBuffer
      curFileBuf <- use fileBuffer
      when (V.slice (fromInteger (curFileOff - curMOff)) (V.length findbuf) curFileBuf == findbuf) $ replaceAtIndex 0

replaceAtIndex :: Int -> EventM AppName AppState ()
replaceAtIndex idx = do
                      rstr <- use replaceString
                      if idx >= length rstr then return ()
                      else do
                        alterAscii (rstr !! idx)
                        hexOffset .= 0 >> alterOffset 1
                        replaceAtIndex (idx + 1)

replaceHexHandler :: EventM AppName AppState ()
replaceHexHandler = do
  mmapFile <- use file
  case mmapFile of
    Nothing -> setStatus "Cannot replace Hex: no file opened"
    Just _ -> do
      findHexOff
      rstr <- use replaceString
      let trimOff = trimStr rstr
          result = readHex trimOff
        in case result of
        [] -> setStatus $ "Cannot parse hex input: " ++ trimOff
        _ ->
          let (_, rest) = head result
            in if rest == "" then
              if length trimOff `mod` 2 == 1
                  then setStatus $ "Cannot parse hex input: " ++ trimOff
                  else do
                    replaceString .= word8VectorToString (convertHex trimOff)
                    curFileOff <- use fileOffset
                    curMOff <- use mmapOffset
                    findbuf <- use findBuffer
                    curFileBuf <- use fileBuffer
                    when (V.slice (fromInteger (curFileOff - curMOff)) (V.length findbuf) curFileBuf == findbuf) $ replaceAtIndex 0
                    replaceString .= rstr
            else setStatus $ "Cannot parse hex input: " ++ rest

word8VectorToString :: V.Vector Word8 -> String
word8VectorToString vec = V.foldl (++) "" (V.map (\n -> [(chr . fromIntegral) n]) vec)

alterAscii :: Char -> EventM AppName AppState ()
alterAscii c = do
  fileBuf <- use fileBuffer
  modificationBuf <- use modificationBuffer
  off <- use fileOffset
  mmapOff <- use mmapOffset
  when (fromInteger (off - mmapOff) < V.length fileBuf) $ fileBuffer .= fileBuf // [(fromInteger (off - mmapOff), (toEnum . fromEnum) c)]
  modificationBuffer .= M.insert off ((toEnum . fromEnum) c) modificationBuf

alterOffset :: Integer -> EventM AppName AppState ()
alterOffset delta = do
  off <- use fileOffset
  jumpOffset $ off + delta


findFromStart :: Integer -> Integer -> Bool -> Bool -> EventM AppName AppState ()
findFromStart originOff originMOff isFst isFromBegin = do
  findBuf <- use findBuffer
  curFileBuf <- use fileBuffer
  mmpOff <- use mmapOffset

  let startIdx = originOff - mmpOff
      idx = if not isFst then isSubVector findBuf curFileBuf 0 else isSubVector findBuf curFileBuf (fromInteger (startIdx + 1))
  prevFileOff <- use fileOffset
  --setStatus $ "current File Offset: " ++ show prevFileOff ++ "!current mmapOff: " ++ show mmpOff ++ "! isFST?" ++ show isFst

  case idx of
    Just i -> let off = (mmpOff + fromIntegral i) in
      jumpOffset off >> setStatus ("Found at offset: " ++ show off)
    Nothing -> do
      curmOff <- use mmapOffset
      jumpOffset (curmOff + fromIntegral (V.length curFileBuf))
      curFileOff <- use fileOffset
      if curFileOff == prevFileOff then
        if isFromBegin then jumpOffset originOff >> setStatus "Not found"
        else do
          jumpOffset 0
          findFromStart originOff originMOff False True
      else
        if isFromBegin && curFileOff > originMOff then jumpOffset originOff >> setStatus "Not found"
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

clearModBuffer :: EventM AppName AppState ()
clearModBuffer = modificationBuffer .= M.empty

closeFile :: EventM AppName AppState ()
closeFile = do
  clearBuffer
  closeMmap
  clearModBuffer
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
  currFile .= ""
  modificationBuffer .= M.empty
  mode .= Cmd
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
      let vp = viewportScroll SaveInput
       in hScrollToEnd vp

findAndReplaceHandler :: BrickEvent AppName () -> EventM AppName AppState ()
findAndReplaceHandler event = case event of
  VtyEvent (EvKey key modifier) -> case modifier of
    [x] -> when (x == MCtrl) $ do
      originalMode <- use findReplaceMode
      findReplaceMode .= not originalMode
      --setStatus $ "Shifted" ++ (show originalMode)
    [] -> case key of
      KChar c -> do
        frMode <- use findReplaceMode
        if frMode then do
          toFind <- use findString
          findString .= toFind ++ [c]
        else do
          toReplace <- use replaceString
          replaceString .= toReplace ++ [c]
        scroll
      KBS -> do
        frMode <- use findReplaceMode
        if frMode then do
          toFind <- use findString
          unless (toFind == "") $
            findString .= init toFind >> scroll
        else do
          toReplace <- use replaceString
          unless (toReplace == "") $
            replaceString .= init toReplace >> scroll
      KDel -> do
        fmode <- use findReplaceMode
        if fmode then
          findString .= ""
        else
          replaceString .= ""
      _ -> return ()
    _ -> return ()
  VtyEvent EvResize {} -> scroll
  _ -> return ()
  where
    scroll =
      let vp1 = viewportScroll FindInput
          vp2 = viewportScroll ReplaceInput
        in
          hScrollToEnd vp1 >> hScrollToEnd vp2

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
jumpPrompt = prompt
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
              let offset = state ^. enterOffset
               in foldl1'
                    (<=>)
                    [ str "Enter offset in hex:",
                      vLimit 1 $ viewport JumpInput Horizontal $ str offset,
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
