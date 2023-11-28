module UI (start) where

import AppData
import Brick
import Brick.Widgets.Center
import Control.Lens
import Control.Monad
import Data.List (foldl')
import qualified Data.Map as M
import Graphics.Vty
import Lib

app :: App AppState e AppName
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
menuText state = drawMenu hl (<+>) (head menuList) (-1)
    where
        hl = if state ^. mode == Cmd
            then
                last $ state ^. menuFocus
            else
                -1

drawMenu :: Int -> (Widget AppName -> Widget AppName -> Widget AppName) -> [MenuItem] -> Int -> Widget AppName
drawMenu hl o items width = result
    where
        (_, result) = foldl' f (0, emptyWidget) items
        itemText x = padLeft (Pad 1) $ padRight (Pad r) $ str repr
            where
                repr = show x
                r = if width < 0
                    then
                        1
                    else
                        width - length repr - 1
        f (i, prev) x = (i + 1, o prev (
            if i == hl
                then
                    withAttr menuSelAttr $ itemText x
                else
                    withAttr menuNormAttr $ itemText x
            ))

genMenuLayers :: AppState -> [Widget AppName]
genMenuLayers state = map f $ zip pos (state ^. menuLayers)
    where
        pos = getMenuPos (state ^. menuFocus) (state ^. menuLayers)
        f (loc, (_, widget)) = Widget Fixed Fixed (
            render $ translateBy (Location loc) $ withAttr menuBgAttr widget)

drawUI :: AppState -> [Widget AppName]
drawUI state = menuLayers ++ [menuWidget, statusWidget, editorWidget]
    where
        statusText = withAttr statusAttr $
            str $ state ^. status
        menuWidget = withAttr menuBgAttr $ vLimit 1 $ viewport Menu Horizontal $ menuText state
        menuLayers = genMenuLayers state
        statusWidget = Widget Greedy Greedy $ do
            ctx <- getContext
            render $ translateBy (Location (0, windowHeight ctx - 1)) $
                withAttr statusBgAttr $ vLimit 1 $ viewport Status Horizontal statusText
        editorWidget = case state ^. file of
            Just path -> undefined
            --Nothing -> withAttr editorBgAttr $ padTopBottom 1 $ viewport Editor Horizontal infoText
            Nothing -> withAttr editorBgAttr $ padTopBottom 1 infoText

handler :: BrickEvent AppName e -> EventM AppName AppState ()
handler event = do
    curr <- use mode
    case curr of
        Cmd -> cmdHandler event
        Edit -> editHandler event

cmdHandler :: BrickEvent AppName e -> EventM AppName AppState ()
cmdHandler (VtyEvent (EvKey key modifier)) = case key of
    KEsc -> do
        mLayers <- use menuLayers
        case mLayers of
            [] -> do
                path <- use file
                case path of
                    Just p -> mode .= Edit
                    Nothing -> status .= "No file opened"
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
    _ -> status .= "Unknown key: " ++ show key
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
        in
            case group of
                Just sub -> addMenu sub 0
                Nothing -> execAction name

getMenuPos :: [Int] -> [(Int, Widget AppName)] -> [(Int, Int)]
getMenuPos = go
    where
        go [_] [] = []
        go (_:[f]) [_] = [(menuRootX !! f, 1)]
        go (_:fs@(f:_)) (_:ls@((i, _):_)) = let rest@((x, y):_) = go fs ls in
            (x + menuLayerWidth !! i, y + f):rest
        go _ _ = undefined

addMenu :: Int -> Int -> EventM AppName AppState ()
addMenu sub focus = do
    oldFocus <- use menuFocus
    menuFocus .= focus:oldFocus
    oldLayers <- use menuLayers
    menuLayers .= newLayer:oldLayers
    where
        newLayer = (sub, withAttr menuBgAttr $
            drawMenu focus (<=>) (menuList !! sub) (menuLayerWidth !! sub))

removeMenu :: EventM AppName AppState ()
removeMenu = do
    oldFocus <- use menuFocus
    oldLayers <- use menuLayers
    let _:fs = oldFocus
        _:ls = oldLayers
        in do
            menuFocus .= fs
            menuLayers .= ls

focusChange :: Int -> EventM AppName AppState ()
focusChange num = do
    mFocus <- use menuFocus
    mLayers <- use menuLayers
    let f:_ = mFocus
        f' id = (f + num) `mod` length (menuList !! id)
        in
            case mLayers of
                [] -> menuFocus .= [f' 0]
                (i, _):_ -> do
                    removeMenu
                    addMenu i (f' i)

execAction :: String -> EventM AppName AppState ()
execAction name = do
    case M.lookup name funcMap of
        Just f -> f
        Nothing -> status .= "Not implemented"
    clearMenus

clearMenus :: EventM AppName AppState ()
clearMenus = do
    mFocus <- use menuFocus
    menuFocus .= [last mFocus]
    menuLayers .= []

editHandler :: BrickEvent AppName e -> EventM AppName AppState ()
editHandler _ = return ()

start :: IO ()
start = void $ defaultMain app initState
