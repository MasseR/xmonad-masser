{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module XMonad.Config.MasseR.Bindings (keybindings) where

import XMonad.Config.MasseR.ExtraConfig

import Data.Tree
       (Tree(Node))

import XMonad
       ( KeyMask
       , KeySym
       , X
       , XConfig
       , className
       , def
       , kill
       , recompile
       , sendMessage
       , spawn
       , stringProperty
       , terminal
       , windows
       , (=?)
       )
import XMonad.Actions.Navigation2D
       (Direction2D(..), windowGo, windowSwap)
import XMonad.Actions.Search
       (google, promptSearchBrowser, searchEngine)
import XMonad.Actions.TreeSelect
       (TSNode(..), treeselectAction, tsDefaultConfig)
import XMonad.Layout
       (ChangeLayout(..))
import XMonad.Layout.ToggleLayouts
       (ToggleLayout(..))
import XMonad.Password
       (passPrompt)
import XMonad.Prompt
       (XPConfig(..))
import XMonad.Prompt.RunOrRaise
       (runOrRaisePrompt)
import XMonad.Prompt.Shell
       (shellPrompt)
import qualified XMonad.StackSet as W
import XMonad.TopicSpace
       ( addWorkspacePrompt
       , copyTopic
       , currentTopicAction'
       , gridselectMove
       , removeEmptyWorkspace
       , topicConfig
       , visualSelect
       )
import XMonad.Util.EZConfig
       (mkNamedKeymap)
import XMonad.Util.NamedActions
       (NamedAction, addName, submapName, subtitle, (^++^))
import XMonad.Util.NamedScratchpad
       (NamedScratchpad(..), namedScratchpadAction, nonFloating)

import Control.Lens
       ((^.))
import Data.Generics.Product
       (field)

xpconf :: XPConfig
xpconf = def{font="xft:Inconsolate-9"}

scratchpads :: ExtraConfig -> [NamedScratchpad]
scratchpads extraConf =
  [ NS "notes" (vimPath <> " -g --role notes -c 'e ~/wikidata/QuickNote.md'") (wmRole =? "notes") nonFloating
  , NS "music" spotifyPath (className =? "Spotify") nonFloating
  ]
    where wmRole = stringProperty "WM_WINDOW_ROLE"
          vimPath = extraConf ^. field @"applications" . field @"vim"
          spotifyPath = extraConf ^. field @"applications" . field @"spotify"

scratchSubmaps :: ExtraConfig -> XConfig l -> NamedAction
scratchSubmaps extraConf conf = submapName . mkNamedKeymap conf $
  [ ("M-n", addName "Open notes" $ namedScratchpadAction scratch "notes")
  , ("s", addName "Open Spotify" $ namedScratchpadAction scratch "music")
  ]
  where
    scratch = scratchpads extraConf

-- Search engines inside submaps
searchSubmaps :: ExtraConfig -> XConfig l -> NamedAction
searchSubmaps extraConfig conf =
    let mkBrowser = promptSearchBrowser xpconf (extraConfig ^. field @"applications" . field @"browser")
        googleP = addName "Search google" $ mkBrowser google
        extras = [(key, addName name $ mkBrowser (searchEngine name url)) | Search{..} <- searchEndpoints extraConfig]
    in submapName . mkNamedKeymap conf $
            ("g", googleP) : extras

spotifySubmap :: ExtraConfig -> XConfig l -> NamedAction
spotifySubmap extraConf conf = submapName . mkNamedKeymap conf $
   [ ("M-p", addName "Play" $ spawn (musicToggle . applications $ extraConf)) ]

projectKeys :: ExtraConfig -> XConfig l -> [(String, NamedAction)]
projectKeys extraConfig conf = [ ("M-y", addName "Change topic" $ visualSelect (topicConfig extraConfig))
                              , ("M-S-g", addName "Move window to topic" $ gridselectMove def)
                              , ("M-<Return>", addName "Open project action" $ currentTopicAction' (topicConfig extraConfig))
                              , ("M-w", modifyWorkspaces)]
  where
    modifyWorkspaces =
      submapName $ mkNamedKeymap conf [ ("a", addName "Add a new workspace" $ addWorkspacePrompt xpconf)
                                      , ("w", addName "Copy project" copyTopic)
                                      , ("d", addName "Remove empty workspace" removeEmptyWorkspace)]

systemTree :: X ()
systemTree = treeselectAction tsDefaultConfig
  [ Node (TSNode "Sleep" "Suspend the machine" (spawn "systemctl suspend")) []
  , Node (TSNode "Shutdown" "Shutdown system" (spawn "shutdown -h now")) []
  ]

keybindings :: ExtraConfig -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keybindings extraConfig conf =
    let subKeys str ks = subtitle str : mkNamedKeymap conf ks in
    subKeys "Actions" [ ("M-S-r", addName "Recompile and restart" (recompile True >> spawn "xmonad --restart"))
                      , ("M-C-l", addName "Lock screen" $ spawn locker)] ^++^
    subKeys "System" [ ("<XF86Sleep>", addName "Suspend machine" $ spawn "sudo pm-suspend")
                     , ("<XF86AudioRaiseVolume>", addName "Increase volume" $ spawn "amixer set Master 2%+")
                     , ("<XF86AudioLowerVolume>", addName "Decrease volume" $ spawn "amixer set Master 2%-")
                     , ("<XF86Favorites>", addName "System commands" systemTree)
                     , ("M-<plus>", addName "Increase volume" $ spawn "amixer set Master 2+")
                     , ("M-<minus>", addName "Decrease volume" $ spawn "amixer set Master 2-")
                     , ("<XF86AudioPlay>", addName "Play/pause music" $ spawn (musicToggle . applications $ extraConfig))
                     , ("M-m", spotifySubmap extraConfig conf)
                     -- , ("M-S-<Space>", addName "Swap screens" swapNextScreen)
                     , ("M-<Backspace>", addName "Kill window" kill)
                     -- scrot requires `unGrab`
                     , ("M-<Print>", addName "Take screenshot" $ spawn (screenshot . applications $ extraConfig))] ^++^
    subKeys "Launchers" [ ("M-S-<Return>", addName "Open terminal" $ spawn $ terminal conf)
                        , ("M-n", (scratchSubmaps extraConfig) conf)
                        , ("M-s", searchSubmaps extraConfig conf)
                        , ("M-p", addName "Retrieve password" $ passPrompt xpconf)
                        , ("M-e", addName "Run app" $ runOrRaisePrompt xpconf)
                        , ("M-S-e", addName "Run shell command" $ shellPrompt xpconf)
                        ] ^++^
    subKeys "Windows" [ ("M-j", addName "Go down" $ windowGo D False)
                      , ("M-k", addName "Go up" $ windowGo U False)
                      , ("M-h", addName "Go left" $ windowGo L False)
                      , ("M-l", addName "Go right" $ windowGo R False)
                      -- Swap screen left or right, don't wrap
                      -- , ("M-S-h", addName "Shift window up" $ screenSwap L True)
                      -- , ("M-S-l", addName "Shift window right" $ screenSwap R True)
                      -- I *was* using these mappings 🤦
                      , ("M-S-j", addName "Shift window down" $ windowSwap D False)
                      , ("M-S-k", addName "Shift window up" $ windowSwap U False)
                      , ("M-S-h", addName "Shift window left" $ windowSwap L False)
                      , ("M-S-l", addName "Shift window right" $ windowSwap R False)
                      , ("M-.", addName "Go to previous window" $ windows W.focusDown)
                      , ("M-,", addName "Go to next window" $ windows W.focusUp)
                      , ("M-S-m", addName "Swap master" $ windows W.swapMaster)
                      ] ^++^
    subKeys "Projects & Workspaces" (projectKeys extraConfig conf) ^++^
    subKeys "Layout management" [ ("M-C-<Space>", addName "Toggle layout" $ sendMessage ToggleLayout)
                                , ("M-z", addName "Toggle zoom" $ sendMessage (Toggle "Zoom"))
                                , ("M-<Space>", addName "Next layout" $ sendMessage NextLayout)] ^++^
    subKeys "Resize" []
  where
    locker = "xset s activate"
