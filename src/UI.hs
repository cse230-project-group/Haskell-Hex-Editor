module UI (start) where

import AppData
import Brick hiding (zoom)
import Brick.Widgets.Center
import Control.Lens
import Control.Monad
import Data.List (foldl', foldl1')
import Data.Maybe
import Graphics.Vty
import Lib

import qualified Data.Map as M

app :: App AppState () AppName
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handler
    , appStartEvent = return ()
    , appAttrMap = const appAttr
    }

infoText :: Widget AppName
infoText = hCenter $ vCenter $ withAttr infoAttr $
    str "Use menu above to open files." <=>
    str "Status is shown at the bottom."

menuText :: AppState -> Widget AppName
menuText state = drawMenu hl (<+>) (head menuList) (-1) 0
    where
        hl = if state ^. mode == Cmd
            then
                last $ state ^. menuFocus
            else
                -1

drawMenu :: Int -> (Widget AppName -> Widget AppName -> Widget AppName) -> [MenuItem] -> Int -> Int -> Widget AppName
drawMenu hl o items width id = result
    where
        (_, result) = foldl' f (0, emptyWidget) items
        itemText x w = padLeft (Pad 1) $ padRight (Pad r) $ str repr
            where
                repr = show x
                r = if w < 0
                    then
                        1
                    else
                        w - length repr - 1
        f (i, prev) x = (i + 1, o prev (Widget Fixed Fixed $ do
            ctx <- getContext
            render $
                let w = min width $ availWidth ctx in
                    if i == hl
                        then
                            reportExtent (MenuBtn id) $ visible $ withAttr menuSelAttr $ itemText x w
                        else
                            withAttr menuNormAttr $ itemText x w
            ))

genMenuLayers :: AppState -> [Widget AppName]
genMenuLayers state = case mLayers of
    [] -> []
    _ -> f' (last mLayers) : zipWith (curry f) (parentIds mLayers) (init mLayers)
    where
        mLayers = state ^. menuLayers
        parentIds (_:ls) = map fst ls
        f' (i, widget) = Widget Fixed Fixed $ do
            ctx <- getContext
            let maxHeight = windowHeight ctx - 1
                maxWidth = windowWidth ctx
                height = length (menuList !! i)
                width = menuLayerWidth !! i
                (px, _) = fromMaybe undefined (M.lookup 0 (state ^. menuBtns))
                x = max (-px) (min 0 (maxWidth - px - width))
                in
                    render $ relativeTo (MenuBtn 0) (Location (x, 1)) $
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
                in
                    render $ relativeTo (MenuBtn p) (Location (x, y)) $
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
            Just (Extent _ (Location (x, y)) _) -> if id == 0
                then
                    (x, y + 1)
                else
                    (x, y)
        in
            menuBtns .= M.insert id loc oldBtns

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
            titleStr = if titleStrLen + 2 <= width
                then
                    p ^. pTitle
                else
                    take (width - 5) (p ^. pTitle) ++ take (min 3 (width - 2)) "..."
            r = width - length titleStr - 1
            title = withAttr promptTitleAttr $ vLimit 1 $ padLeft (Pad 1) $ padRight (Pad r) $ str titleStr
            body = withAttr promptBgAttr $ vLimit height $ viewport PromptBody Both $ Widget Fixed Fixed $
                render $ withAttr promptAttr $ p ^. pBody
            btnDisplay = if sum (map ((+) 2 . length . fst) (p ^. pButtons)) > width
                then
                    viewport PromptBtnLayer Horizontal
                else
                    hCenter
            buttons = withAttr promptBgAttr $ vLimit 1 $ btnDisplay $
                foldl1' (<+>) $ zipWith (curry genButton) [0..] (p ^. pButtons)
            genButton (id, (s, _)) =
                let alt = if id == p ^. pButtonFocus
                        then
                            withAttr menuSelAttr . visible
                        else
                            withAttr menuNormAttr
                    in
                        alt $ padLeft (Pad 1) $ padRight (Pad 1) $ str s
            in
                render $ translateBy (Location (x, y)) $ hLimit width $
                    title <=>
                    body <=>
                    buttons

drawUI :: AppState -> [Widget AppName]
drawUI state = promptLayer : menuLayers ++ [menuWidget, statusWidget, editorWidget]
    where
        statusText = withAttr statusAttr $
            str $ state ^. status
        promptLayer = fromMaybe emptyWidget $ drawPrompt state
        menuWidget = withAttr menuBgAttr $ vLimit 1 $ viewport Menu Horizontal $ menuText state
        menuLayers = reverse $ genMenuLayers state
        statusWidget = Widget Greedy Greedy $ do
            ctx <- getContext
            render $ translateBy (Location (0, windowHeight ctx - 1)) $
                withAttr statusBgAttr $ vLimit 1 $ viewport Status Horizontal statusText
        editorWidget = case state ^. file of
            Just path -> undefined
            --Nothing -> withAttr editorBgAttr $ padTopBottom 1 $ viewport Editor Horizontal infoText
            Nothing -> withAttr editorBgAttr $ padTopBottom 1 infoText

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
            _:ls -> do
                menuLayers .= ls
                mFocus <- use menuFocus
                let _:fs = mFocus in
                    menuFocus .= fs
    KUp -> focusChange (-1)
    KLeft -> focusChange (-1)
    KDown -> focusChange 1
    KRight -> focusChange 1
    KEnter -> menuHandler
    KPageUp -> statusScroll (-1)
    KPageDown -> statusScroll 1
    _ -> setStatus $ "Unknown key: " ++ show key
cmdHandler (VtyEvent EvResize {}) = do
    --refreshStatus
    refreshBtn 0
    mLayers <- use menuLayers
    mapM_ ((\i ->
        let vp = viewportScroll (MenuLayer i) in do
            vScrollToBeginning vp
            refreshBtn i) . fst) (reverse mLayers)
cmdHandler _ = return ()

menuHandler :: EventM AppName AppState ()
menuHandler = do
    mFocus <- use menuFocus
    mLayers <- use menuLayers
    let focus = head mFocus
        id = case mLayers of
            [] -> 0
            ((i, _):_) -> i
        MkMenu name group = menuList !! id !! focus
        in do
            refreshBtn id
            case group of
                Just sub -> addMenu sub 0
                Nothing -> execAction name

addMenu :: Int -> Int -> EventM AppName AppState ()
addMenu sub focus = do
    oldFocus <- use menuFocus
    menuFocus .= focus:oldFocus
    oldLayers <- use menuLayers
    menuLayers .= newLayer:oldLayers
    where
        newLayer = (sub, withAttr menuBgAttr $
            drawMenu focus (<=>) (menuList !! sub) (menuLayerWidth !! sub) sub)

removeMenu :: EventM AppName AppState ()
removeMenu = do
    oldFocus <- use menuFocus
    oldLayers <- use menuLayers
    oldBtns <- use menuBtns
    let _:fs = oldFocus
        (i, _):ls = oldLayers
        in do
            menuFocus .= fs
            menuLayers .= ls
            menuBtns .= M.delete i oldBtns

focusChange :: Int -> EventM AppName AppState ()
focusChange num = do
    mFocus <- use menuFocus
    mLayers <- use menuLayers
    let f:_ = mFocus
        f' id = (f + num) `mod` length (menuList !! id)
        in
            case mLayers of
                [] -> do
                    menuFocus .= [f' 0]
                    refreshBtn 0
                (i, _):_ -> do
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
    let loc = fromMaybe undefined $ M.lookup 0 mBtns in
        menuBtns .= M.fromList [(0, loc)]

statusScroll :: Int -> EventM AppName AppState ()
statusScroll d = let vp = viewportScroll Status in
    hScrollBy vp d

promptHandler :: BrickEvent AppName () -> EventM AppName AppState ()
promptHandler (VtyEvent (EvKey key modifier)) = case key of
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
    KUp -> let vp = viewportScroll PromptBody in
        vScrollBy vp (-1)
    KDown -> let vp = viewportScroll PromptBody in
        vScrollBy vp 1
    KLeft -> let vp = viewportScroll PromptBody in
        hScrollBy vp (-1)
    KRight -> let vp = viewportScroll PromptBody in
        hScrollBy vp 1
    _ -> return ()
promptHandler _ = return ()

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
editHandler _ = return ()

start :: IO ()
start = void $ defaultMain app initState
