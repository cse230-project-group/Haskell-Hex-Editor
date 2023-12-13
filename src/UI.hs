module UI (start) where

import AppData
import Brick hiding (zoom)
import Brick.Widgets.Center
import Control.Lens
import Control.Monad
import Data.Bits
import Data.Char (chr, isHexDigit, toUpper)
import Data.List (foldl', foldl1')
import qualified Data.Map as M
import Data.Maybe
import Data.Vector ((!), (//))
import GHC.Num (integerLog2)
import Graphics.Vty
import Lib
import Numeric (readHex, showHex)

app :: App AppState () AppName
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handler,
      appStartEvent = return (),
      appAttrMap = const appAttr
    }

infoText :: Widget AppName
infoText =
  hCenter $
    vCenter $
      withAttr infoAttr $
        str "Use menu above to open files."
          <=> str "Status is shown at the bottom."

menuText :: AppState -> Widget AppName
menuText state = drawMenu hl (<+>) (head menuList) (-1) 0
  where
    hl =
      if state ^. mode == Cmd
        then last $ state ^. menuFocus
        else -1

drawMenu :: Int -> (Widget AppName -> Widget AppName -> Widget AppName) -> [MenuItem] -> Int -> Int -> Widget AppName
drawMenu hl o items width id = result
  where
    (_, result) = foldl' f (0, emptyWidget) items
    itemText x w = padLeft (Pad 1) $ padRight (Pad r) $ str repr
      where
        repr = show x
        r =
          if w < 0
            then 1
            else w - length repr - 1
    f (i, prev) x =
      ( i + 1,
        o
          prev
          ( Widget Fixed Fixed $ do
              ctx <- getContext
              render $
                let w = min width $ availWidth ctx
                 in if i == hl
                      then reportExtent (MenuBtn id) $ visible $ withAttr menuSelAttr $ itemText x w
                      else withAttr menuNormAttr $ itemText x w
          )
      )

genMenuLayers :: AppState -> [Widget AppName]
genMenuLayers state = case mLayers of
  [] -> []
  _ -> f' (last mLayers) : zipWith (curry f) (parentIds mLayers) (init mLayers)
  where
    mLayers = state ^. menuLayers
    parentIds (_ : ls) = map fst ls
    f' (i, widget) = Widget Fixed Fixed $ do
      ctx <- getContext
      let maxHeight = windowHeight ctx - 1
          maxWidth = windowWidth ctx
          height = length (menuList !! i)
          width = menuLayerWidth !! i
          (px, _) = fromMaybe undefined (M.lookup 0 (state ^. menuBtns))
          x = max (-px) (min 0 (maxWidth - px - width))
       in render $
            relativeTo (MenuBtn 0) (Location (x, 1)) $
              vLimit (min maxHeight height) $
                hLimit (min maxWidth width) $
                  viewport (MenuLayer i) Vertical $
                    withAttr menuBgAttr widget
    f (p, (i, widget)) = Widget Fixed Fixed $ do
      ctx <- getContext
      let maxHeight = windowHeight ctx - 1
          maxWidth = windowWidth ctx
          height = length (menuList !! i)
          width = menuLayerWidth !! i
          (px, py) = fromMaybe undefined (M.lookup p (state ^. menuBtns))
          x = max (-px) (min (menuLayerWidth !! p) (maxWidth - px - width))
          y = max (-py + 1) (min 0 (maxHeight - py - height))
       in render $
            relativeTo (MenuBtn p) (Location (x, y)) $
              vLimit (min maxHeight height) $
                hLimit (min maxWidth width) $
                  viewport (MenuLayer i) Vertical $
                    withAttr menuBgAttr widget

refreshBtn :: Int -> EventM AppName AppState ()
refreshBtn id = do
  extent <- lookupExtent (MenuBtn id)
  oldBtns <- use menuBtns
  let loc = case extent of
        Nothing -> undefined
        Just (Extent _ (Location (x, y)) _) ->
          if id == 0
            then (x, y + 1)
            else (x, y)
   in menuBtns .= M.insert id loc oldBtns

drawPrompt :: AppState -> Maybe (Widget AppName)
drawPrompt state = case state ^. prompt of
  Nothing -> Nothing
  Just p -> return $ Widget Fixed Fixed $ do
    ctx <- getContext
    let width = min (windowWidth ctx) (fromMaybe (windowWidth ctx `div` 2) $ p ^. pWidth)
        height = min (windowHeight ctx - 2) (fromMaybe ((windowHeight ctx - 2) `div` 2) $ p ^. pHeight)
        x = (windowWidth ctx - width) `div` 2
        y = (windowHeight ctx - height - 2) `div` 2
        titleStrLen = length $ p ^. pTitle
        titleStr =
          if titleStrLen + 2 <= width
            then p ^. pTitle
            else take (width - 5) (p ^. pTitle) ++ take (min 3 (width - 2)) "..."
        r = width - length titleStr - 1
        title = withAttr promptTitleAttr $ vLimit 1 $ padLeft (Pad 1) $ padRight (Pad r) $ str titleStr
        body =
          withAttr promptBgAttr $
            vLimit height $
              viewport PromptBody Both $
                Widget Fixed Fixed $
                  render $
                    withAttr promptAttr $
                      (p ^. pBody) state (width, height)
        btnDisplay =
          if sum (map ((+) 2 . length . fst) (p ^. pButtons)) > width
            then viewport PromptBtnLayer Horizontal
            else hCenter
        buttons =
          withAttr promptBgAttr $
            vLimit 1 $
              btnDisplay $
                foldl1' (<+>) $
                  zipWith (curry genButton) [0 ..] (p ^. pButtons)
        genButton (id, (s, _)) =
          let alt =
                if id == p ^. pButtonFocus
                  then withAttr menuSelAttr . visible
                  else withAttr menuNormAttr
           in alt $ padLeft (Pad 1) $ padRight (Pad 1) $ str s
     in render $
          translateBy (Location (x, y)) $
            hLimit width $
              title
                <=> body
                <=> buttons

drawEditor :: AppState -> Widget AppName
drawEditor state = translateBy (Location (0, 1)) $ withAttr editorBgAttr $ reportExtent Editor $ offsetWidget <+> hexWidget <+> asciiWidget
  where
    offsetWidget = Widget Fixed Fixed $ do
      ctx <- getContext
      let height = windowHeight ctx - 2
          currRow = state ^. fileOffset `div` 16
          minRow = state ^. fileRow
          maxRow = min ((max 1 (state ^. fileSize) - 1) `div` 16) $ minRow + fromIntegral height - 2
          rawWidth = ((fromIntegral (integerLog2 maxRow) + 4) `div` 16 + 1) * 4 + 2
          widthLimit = windowWidth ctx `div` 2
          width =
            if rawWidth > widthLimit
              then max 6 $ rawWidth - ((rawWidth - widthLimit - 1) `div` 4 + 1) * 4
              else rawWidth
          f num =
            let rawNum = replicate (width - 4) '0' ++ showHex num "0"
                attr =
                  if num == currRow
                    then headerFocusAttr
                    else headerAttr
             in withAttr attr $ str $ map toUpper $ drop (length rawNum + 2 - width) rawNum
       in render $
            vLimit height $
              hLimit width $
                padLeftRight 1 $
                  if maxRow < minRow
                    then str $ replicate (width - 2) ' '
                    else padTop (Pad 1) $ foldl1' (<=>) $ map f [minRow .. maxRow]
    -- render $ str (show (height, currRow, minRow, maxRow, rawWidth, widthLimit, width))
    hexHeader = foldl1' (<+>) $ f (head lst) : map (padLeft (Pad 1) . f) (tail lst)
      where
        lst = map (\n -> (n, ' ' : map toUpper (showHex n ""))) [0 .. 15]
        f (n, e) =
          let attr =
                if state ^. fileOffset `mod` 16 == n
                  then withAttr headerFocusAttr . visible
                  else withAttr headerAttr
           in attr $ str e
    hexWidget = Widget Fixed Fixed $ do
      ctx <- getContext
      let height = windowHeight ctx - 2
          minRow = state ^. fileRow
          maxRow = min ((max 1 (state ^. fileSize) - 1) `div` 16) $ minRow + fromIntegral height - 2
          leftRawWidth = ((fromIntegral (integerLog2 maxRow) + 4) `div` 16 + 1) * 4 + 2
          leftWidthLimit = windowWidth ctx `div` 2
          leftWidth =
            if leftRawWidth > leftWidthLimit
              then max 6 $ leftRawWidth - ((leftRawWidth - leftWidthLimit - 1) `div` 4 + 1) * 4
              else leftRawWidth
          width = (windowWidth ctx - leftWidth) * 3 `div` 4
          f :: Integer -> Widget AppName
          f num =
            let begin = num * 16
                end = min (state ^. fileSize - 1) $ begin + 15
                g :: Integer -> Widget AppName
                g off =
                  let focusAttr = if state ^. hexMode then editorFocusAttr else editorWeakFocusAttr
                      focusModAttr = if state ^. hexMode then editorModWeakFocusAttr else editorModFocusAttr
                      (attr, hexRaw) = case M.lookup off (state ^. modificationBuffer) of
                        Just byte -> (if state ^. fileOffset == off
                          then foldl1' (<+>) $ zipWith (curry $ fun (focusModAttr, editorModAttr)) [0 ..] hexStr
                          else withAttr editorModAttr $ str hexStr
                          , map toUpper $ showHex byte "")
                        Nothing -> (if state ^. fileOffset == off
                          then foldl1' (<+>) $ zipWith (curry $ fun (focusAttr, editorAttr)) [0 ..] hexStr
                          else withAttr editorAttr $ str hexStr
                          , map toUpper $ showHex ((state ^. fileBuffer) ! fromInteger (off - (state ^. mmapOffset))) "")
                      hexStr = if length hexRaw == 1 then '0' : hexRaw else hexRaw
                      fun (fAttr, nAttr) (i, c) =
                        (if i == state ^. hexOffset then withAttr fAttr . visible else withAttr nAttr) $
                          str [c]
                   in attr
             in foldl1' (<+>) $ g begin : map (padLeft (Pad 1) . g) [begin + 1 .. end]
       in render $
            vLimit height $
              hLimit width $
                padLeftRight 1 $
                  viewport HexView Horizontal $
                    hexHeader
                      <=> if maxRow < minRow
                        then emptyWidget
                        else foldl1' (<=>) $ map f [minRow .. maxRow]
    asciiHeader width = withAttr headerAttr $ padLeft (Pad (width - 7)) $ str "ASCII"
    asciiWidget = Widget Fixed Fixed $ do
      ctx <- getContext
      let height = windowHeight ctx - 2
          minRow = state ^. fileRow
          maxRow = min ((max 1 (state ^. fileSize) - 1) `div` 16) $ minRow + fromIntegral height - 2
          leftRawWidth = ((fromIntegral (integerLog2 maxRow) + 4) `div` 16 + 1) * 4 + 2
          leftWidthLimit = windowWidth ctx `div` 2
          leftWidth =
            if leftRawWidth > leftWidthLimit
              then max 6 $ leftRawWidth - ((leftRawWidth - leftWidthLimit - 1) `div` 4 + 1) * 4
              else leftRawWidth
          width = (windowWidth ctx - leftWidth) - ((windowWidth ctx - leftWidth) * 3 `div` 4)
          f :: Integer -> Widget AppName
          f num =
            let begin = num * 16
                end = min (state ^. fileSize - 1) $ begin + 15
                g :: Integer -> Widget AppName
                g off =
                  let focusAttr = if state ^. hexMode then editorWeakFocusAttr else editorFocusAttr
                      focusModAttr = if state ^. hexMode then editorModWeakFocusAttr else editorModFocusAttr
                      (attr, asciiVal) = case M.lookup off (state ^. modificationBuffer) of
                        Just byte -> (if state ^. fileOffset == off
                          then withAttr focusModAttr . visible
                          else withAttr editorModAttr
                          , chr $ fromIntegral byte)
                        Nothing -> (if state ^. fileOffset == off
                          then withAttr focusAttr . visible
                          else withAttr editorAttr
                          , chr $ fromIntegral $ (state ^. fileBuffer) ! fromInteger (off - (state ^. mmapOffset)))
                      asciiStr = if asciiVal >= ' ' && asciiVal <= '~' then [asciiVal] else "."
                   in attr $ str asciiStr
             in foldl1' (<+>) $ map g [begin .. end]
       in render $
            vLimit height $
              hLimit width $
                padLeftRight 1 $
                  viewport AsciiView Horizontal $
                    asciiHeader width
                      <=> if maxRow < minRow
                        then emptyWidget
                        else foldl1' (<=>) $ map f [minRow .. maxRow]

drawUI :: AppState -> [Widget AppName]
drawUI state = promptLayer : menuLayers ++ [menuWidget, statusWidget, editorWidget]
  where
    statusText =
      withAttr statusAttr $
        str $
          state ^. status
    promptLayer = fromMaybe emptyWidget $ drawPrompt state
    menuWidget = withAttr menuBgAttr $ vLimit 1 $ viewport Menu Horizontal $ menuText state
    menuLayers = reverse $ genMenuLayers state
    statusWidget = Widget Greedy Greedy $ do
      ctx <- getContext
      render $
        translateBy (Location (0, windowHeight ctx - 1)) $
          withAttr statusBgAttr $
            vLimit 1 $
              viewport Status Horizontal statusText
    editorWidget = case state ^. file of
      Just _ -> drawEditor state
      Nothing -> withAttr editorBgAttr $ padTopBottom 1 $ reportExtent Editor infoText

handler :: BrickEvent AppName () -> EventM AppName AppState ()
handler event = do
  p <- use prompt
  case p of
    Just _ -> promptHandler event
    Nothing -> do
      curr <- use mode
      case curr of
        Cmd -> cmdHandler event
        Edit -> editHandler event

cmdHandler :: BrickEvent AppName () -> EventM AppName AppState ()
cmdHandler (VtyEvent (EvKey key modifier)) = case key of
  KEsc -> do
    mLayers <- use menuLayers
    case mLayers of
      [] -> do
        path <- use file
        case path of
          Just p -> mode .= Edit
          Nothing -> setStatus "No file opened"
      _ : ls -> do
        menuLayers .= ls
        mFocus <- use menuFocus
        let _ : fs = mFocus
         in menuFocus .= fs
  KUp -> focusChange (-1)
  KLeft -> focusChange (-1)
  KDown -> focusChange 1
  KRight -> focusChange 1
  KEnter -> menuHandler
  KPageUp -> statusScroll (-1)
  KPageDown -> statusScroll 1
  _ -> setStatus $ "Unknown key: " ++ show key
cmdHandler (VtyEvent EvResize {}) = do
  -- refreshStatus
  refreshBtn 0
  mLayers <- use menuLayers
  mapM_
    ( ( \i ->
          let vp = viewportScroll (MenuLayer i)
           in do
                vScrollToBeginning vp
                refreshBtn i
      )
        . fst
    )
    (reverse mLayers)
cmdHandler _ = return ()

menuHandler :: EventM AppName AppState ()
menuHandler = do
  mFocus <- use menuFocus
  mLayers <- use menuLayers
  let focus = head mFocus
      id = case mLayers of
        [] -> 0
        ((i, _) : _) -> i
      MkMenu name group = menuList !! id !! focus
   in do
        refreshBtn id
        case group of
          Just sub -> addMenu sub 0
          Nothing -> execAction name

addMenu :: Int -> Int -> EventM AppName AppState ()
addMenu sub focus = do
  oldFocus <- use menuFocus
  menuFocus .= focus : oldFocus
  oldLayers <- use menuLayers
  menuLayers .= newLayer : oldLayers
  where
    newLayer =
      ( sub,
        withAttr menuBgAttr $
          drawMenu focus (<=>) (menuList !! sub) (menuLayerWidth !! sub) sub
      )

removeMenu :: EventM AppName AppState ()
removeMenu = do
  oldFocus <- use menuFocus
  oldLayers <- use menuLayers
  oldBtns <- use menuBtns
  let _ : fs = oldFocus
      (i, _) : ls = oldLayers
   in do
        menuFocus .= fs
        menuLayers .= ls
        menuBtns .= M.delete i oldBtns

focusChange :: Int -> EventM AppName AppState ()
focusChange num = do
  mFocus <- use menuFocus
  mLayers <- use menuLayers
  let f : _ = mFocus
      f' id = (f + num) `mod` length (menuList !! id)
   in case mLayers of
        [] -> do
          menuFocus .= [f' 0]
          refreshBtn 0
        (i, _) : _ -> do
          removeMenu
          addMenu i (f' i)
          refreshBtn i

execAction :: String -> EventM AppName AppState ()
execAction name = do
  case M.lookup name funcMap of
    Just f -> f
    Nothing -> setStatus "Not implemented"
  clearMenus

clearMenus :: EventM AppName AppState ()
clearMenus = do
  mFocus <- use menuFocus
  menuFocus .= [last mFocus]
  menuLayers .= []
  mBtns <- use menuBtns
  let loc = fromMaybe undefined $ M.lookup 0 mBtns
   in menuBtns .= M.fromList [(0, loc)]

statusScroll :: Int -> EventM AppName AppState ()
statusScroll d =
  let vp = viewportScroll Status
   in hScrollBy vp d

promptHandler :: BrickEvent AppName () -> EventM AppName AppState ()
promptHandler event@(VtyEvent (EvKey key modifier)) = case key of
  KChar '\t' -> promptFocusChange 1
  KBackTab -> promptFocusChange (-1)
  KEnter -> do
    p <- use prompt
    let p' = fromMaybe undefined p
     in do
          (_, f) <- nestEventM p' $ do
            focus <- use pButtonFocus
            btns <- use pButtons
            return $ snd $ btns !! focus
          f
  KEsc -> prompt .= Nothing
  KUp ->
    let vp = viewportScroll PromptBody
     in vScrollBy vp (-1)
  KDown ->
    let vp = viewportScroll PromptBody
     in vScrollBy vp 1
  KLeft ->
    let vp = viewportScroll PromptBody
     in hScrollBy vp (-1)
  KRight ->
    let vp = viewportScroll PromptBody
     in hScrollBy vp 1
  _ -> passExtraHandler event
promptHandler event = passExtraHandler event

passExtraHandler :: BrickEvent AppName () -> EventM AppName AppState ()
passExtraHandler event = do
  p <- use prompt
  let p' = fromMaybe undefined p
   in do
        (_, f) <- nestEventM p' $ do
          use pExtraHandler
        case f of
          Just f' -> f' event
          Nothing -> return ()

promptFocusChange :: Int -> EventM n AppState ()
promptFocusChange i = do
  p <- use prompt
  let p' = fromMaybe undefined p
   in do
        p'' <- nestEventM' p' $ do
          focus <- use pButtonFocus
          btns <- use pButtons
          pButtonFocus .= (focus + i) `mod` length btns
        prompt .= Just p''

editHandler :: BrickEvent AppName () -> EventM AppName AppState ()
editHandler event = case event of
  VtyEvent (EvKey key modifier) -> case key of
    KEsc -> mode .= Cmd
    KChar '\t' -> do
      old <- use hexMode
      hexMode .= not old
    KLeft -> do
      off <- use fileOffset
      hexOff <- use hexOffset
      hm <- use hexMode
      if hm
        then unless (off == 0 && hexOff == 0) $ alterHexOffset (-1)
        else hexOffset .= 0 >> alterOffset (-1)
    KRight -> do
      off <- use fileOffset
      hexOff <- use hexOffset
      size <- use fileSize
      hm <- use hexMode
      if hm
        then unless (off == size - 1 && hexOff == 1) $ alterHexOffset 1
        else hexOffset .= 0 >> alterOffset 1
    KUp -> alterOffset (-16)
    KDown -> alterOffset 16
    KHome -> do
      off <- use fileOffset
      alterOffset (-(off `mod` 16))
    KEnd -> do
      off <- use fileOffset
      alterOffset (15 - (off `mod` 16))
    KPageUp -> alterOffsetPage (-1)
    KPageDown -> alterOffsetPage 1
    KChar ch -> do
      off <- use fileOffset
      hexOff <- use hexOffset
      size <- use fileSize
      md <- use hexMode
      if md
        then do
          when (isHexDigit ch) $ alterHex (fst (head (readHex [ch])))
          unless (off == size - 1 && hexOff == 1) $ alterHexOffset 1
        else do
          alterAscii ch
          hexOffset .= 0 >> alterOffset 1
    _ -> return ()
  VtyEvent EvResize {} -> alterOffset 0
  _ -> return ()
  where
    alterHexOffset delta = do
      hexOff <- use hexOffset
      let rawHexOff = hexOff + delta
          deltaOff = fromIntegral rawHexOff `div` 2
          newHexOff = rawHexOff `mod` 2
       in do
            hexOffset .= newHexOff
            unless (deltaOff == 0) $ alterOffset deltaOff
    alterOffset delta = do
      off <- use fileOffset
      jumpOffset $ off + delta
    alterOffsetPage delta = do
      ext <- lookupExtent Editor
      case ext of
        Nothing -> setStatus "internal error"
        Just (Extent _ _ (_, h)) -> alterOffset $ delta * 16 * fromIntegral (h - 1)
    alterHex :: Int -> EventM AppName AppState ()
    alterHex c = do
      fileBuf <- use fileBuffer
      modificationBuf <- use modificationBuffer
      off <- use fileOffset
      hexOff <- use hexOffset
      mmapOff <- use mmapOffset
      let current = case M.lookup off modificationBuf of
            Just byte -> byte
            Nothing -> fileBuf ! fromInteger (off - mmapOff)
          updated =
            if hexOff == 1
              then current .&. 0xF0 .|. fromIntegral c
              else current .&. 0x0F .|. fromIntegral c `shiftL` 4
      fileBuffer .= fileBuf // [(fromInteger (off - mmapOff), updated)]
      modificationBuffer .= M.insert off updated modificationBuf
    alterAscii :: Char -> EventM AppName AppState ()
    alterAscii c = do
      fileBuf <- use fileBuffer
      modificationBuf <- use modificationBuffer
      off <- use fileOffset
      mmapOff <- use mmapOffset
      fileBuffer .= fileBuf // [(fromInteger (off - mmapOff), (toEnum . fromEnum) c)]
      modificationBuffer .= M.insert off ((toEnum . fromEnum) c) modificationBuf

start :: IO ()
start = void $ defaultMain app initState
