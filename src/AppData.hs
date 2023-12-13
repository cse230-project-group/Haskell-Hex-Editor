{-# LANGUAGE TemplateHaskell #-}

module AppData where

import Brick
import Control.Lens (makeLenses)
import qualified Data.Map as M
import Data.Vector (Vector, empty)
import Foreign
import Graphics.Vty

data AppMode = Cmd | Edit
  deriving (Eq)

data AppName
  = Menu
  | Editor
  | Status
  | MenuLayer Int
  | MenuBtn Int
  | PromptBody
  | PromptBtnLayer
  | PromptBtn String
  | OpenInput
  | JumpInput
  | HexView
  | AsciiView
  deriving (Eq, Ord, Show)

data AppPrompt = MkPrompt
  { _pTitle :: String,
    _pBody :: AppState -> (Int, Int) -> Widget AppName,
    _pWidth :: Maybe Int,
    _pHeight :: Maybe Int,
    _pButtons :: [(String, EventM AppName AppState ())],
    _pButtonFocus :: Int,
    _pExtraHandler :: Maybe (BrickEvent AppName () -> EventM AppName AppState ())
  }

data AppState = MkState
  { _mode :: AppMode,
    _file :: Maybe (Ptr Word8, Int, Int, Int),
    _status :: String,
    _menuFocus :: [Int],
    _menuLayers :: [(Int, Widget AppName)],
    _menuBtns :: M.Map Int (Int, Int),
    _prompt :: Maybe AppPrompt,
    _fileRow :: Integer,
    _fileOffset :: Integer,
    _fileSize :: Integer,
    _enterFile :: String,
    _newFile :: String,
    _fileWrite :: Bool,
    _mmapOffset :: Integer,
    _hexOffset :: Int,
    _fileBuffer :: Vector Word8,
    _modificationBuffer :: M.Map Integer Word8,
    _perfCount :: Int,
    _hexMode :: Bool,
    _enterOffset :: String,
    _findString :: String,
    _replaceString :: String
  }

makeLenses ''AppPrompt
makeLenses ''AppState

data MenuItem = MkMenu String (Maybe Int)
  deriving (Eq, Ord)

instance Show MenuItem where
  show (MkMenu name _) = name

initState :: AppState
initState = MkState Cmd Nothing "Ready" [0] [] (M.fromList [(0, (0, 1))]) Nothing 0 0 0 "" "" False (-1) 0 empty M.empty 0 True "" "" ""

menuList :: [[MenuItem]]
menuList =
  [ [ MkMenu "File" (Just 1),
      MkMenu "Edit" (Just 4),
      MkMenu "Help" (Just 2)
    ],
    [ MkMenu "Open ..." Nothing,
      MkMenu "Save" Nothing,
      MkMenu "Save as ..." Nothing,
      MkMenu "Close" Nothing,
      MkMenu "Exit" Nothing
    ],
    [ MkMenu "Debug ->" (Just 3),
      MkMenu "About" Nothing
    ],
    [ MkMenu "Debug Status" Nothing,
      MkMenu "Debug 1" Nothing,
      MkMenu "Debug 2" Nothing,
      MkMenu "Debug 3" Nothing,
      MkMenu "Debug 4" Nothing,
      MkMenu "Debug 5" Nothing,
      MkMenu "Debug Long String .............................. 1 .............. 2 ...... 3 .. 4  5 End" Nothing,
      MkMenu "Debug Prompt" Nothing
    ],
    [ MkMenu "Jump ..." Nothing,
      MkMenu "Find ..." Nothing
    ]
  ]

menuRootWidth :: [Int]
menuRootWidth = map f $ head menuList
  where
    f x = length (show x) + 2

menuRootX :: [Int]
menuRootX = helper [0] 0 menuRootWidth
  where
    helper res _ [x] = res
    helper res acc (x : xs) =
      let acc' = acc + x
       in helper (res ++ [acc']) acc' xs
    helper _ _ [] = undefined

menuLayerWidth :: [Int]
menuLayerWidth = 0 : map f (tail menuList)
  where
    f x = maximum (map (length . show) x) + 2

grey :: Color
grey = linearColor 100 100 100

brightGrey :: Color
brightGrey = linearColor 150 150 150

editorBgAttr :: AttrName
editorBgAttr = attrName "editor-background"

menuBgAttr :: AttrName
menuBgAttr = attrName "menu-background"

statusBgAttr :: AttrName
statusBgAttr = attrName "status-background"

menuNormAttr :: AttrName
menuNormAttr = attrName "menu-normal"

menuSelAttr :: AttrName
menuSelAttr = attrName "menu-selected"

infoAttr :: AttrName
infoAttr = attrName "info"

statusAttr :: AttrName
statusAttr = attrName "status"

promptTitleAttr :: AttrName
promptTitleAttr = attrName "prompt-title"

promptBgAttr :: AttrName
promptBgAttr = attrName "prompt-background"

promptAttr :: AttrName
promptAttr = attrName "prompt"

headerAttr :: AttrName
headerAttr = attrName "header"

headerFocusAttr :: AttrName
headerFocusAttr = attrName "header-focus"

editorAttr :: AttrName
editorAttr = attrName "editor"

editorFocusAttr :: AttrName
editorFocusAttr = attrName "editor-focus"

editorWeakFocusAttr :: AttrName
editorWeakFocusAttr = attrName "editor-weak-focus"

editorModAttr :: AttrName
editorModAttr = attrName "editor-modified"

editorModFocusAttr :: AttrName
editorModFocusAttr = attrName "editor-modified-focus"

editorModWeakFocusAttr :: AttrName
editorModWeakFocusAttr = attrName "editor-modified-weak-focus"

appAttr :: AttrMap
appAttr =
  attrMap
    defAttr
    [ (editorBgAttr, bg black),
      (menuBgAttr, bg white),
      (statusBgAttr, bg brightGrey),
      (menuNormAttr, black `on` white),
      (menuSelAttr, white `on` blue),
      (infoAttr, grey `on` black),
      (statusAttr, black `on` brightGrey),
      (promptTitleAttr, white `on` cyan),
      (promptBgAttr, bg white),
      (promptAttr, black `on` white),
      (headerAttr, blue `on` black),
      (headerFocusAttr, yellow `on` black),
      (editorAttr, white `on` black),
      (editorFocusAttr, white `on` brightGrey),
      (editorWeakFocusAttr, white `on` grey),
      (editorModAttr, red `on` black),
      (editorModFocusAttr, red `on` brightGrey),
      (editorModWeakFocusAttr, red `on` grey)
    ]
