{-# LANGUAGE DataKinds #-}
module XMonad.Config.MasseR.Bindings (keybindings) where



import Data.Tree
       (Tree(Node), Forest)

import XMonad
       ( KeyMask
       , KeySym
       , X
       , XConfig
       , kill
       , sendMessage
       , spawn
       , terminal
       , windows, Default (def), io, withFocused
       )
import XMonad.Actions.Navigation2D
       (Direction2D(..), windowGo, windowSwap)
import XMonad.Actions.TreeSelect
       (TSNode(..), treeselectAction)
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
import XMonad.Util.EZConfig
       (mkNamedKeymap)
import XMonad.Util.NamedActions
       (NamedAction, addName, subtitle, (^++^), submapName)

import XMonad.Actions.CycleWS (swapNextScreen)
import XMonad.Actions.TopicSpace (TopicConfig)
import XMonad.TopicUtils
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import System.Exit (exitSuccess)


projectKeys :: TopicConfig -> XPConfig -> XConfig l -> [(String, NamedAction)]
projectKeys topics xpconf conf = [ ("M-y", addName "Change topic" $ visualSelect topics)
                                 , ("M-S-g", addName "Move window to topic" $ gridselectMove def)
                                 , ("M-<Return>", addName "Open project action" $ currentTopicAction' topics)
                                 , ("M-w", modifyWorkspaces)]
  where
    modifyWorkspaces =
      submapName $ mkNamedKeymap conf [ ("a", addName "Add a new workspace" $ addWorkspacePrompt xpconf)
                                      , ("w", addName "Copy project" copyTopic)
                                      , ("d", addName "Remove empty workspace" removeEmptyWorkspace)]

type Menu = Forest (TSNode (X ()))

systemMenu :: Menu
systemMenu =
  [ Node (TSNode "Sleep" "Suspend the machine" (spawn "systemctl suspend")) []
  , Node (TSNode "Hibernate" "Hibernate the machine" (spawn "systemctl hibernate")) []
  , Node (TSNode "Shutdown" "Shutdown system" (spawn "shutdown -h now")) []
  , Node (TSNode "Log out" "Log out of xmonad" (io exitSuccess)) []
  ]

keybindings :: Menu -> TopicConfig -> XPConfig -> XConfig l -> [((KeyMask, KeySym), NamedAction)]
keybindings menu topics xpconf conf =
    let subKeys str ks = subtitle str : mkNamedKeymap conf ks in
    -- subKeys "Actions" [ ("M-S-r", addName "Recompile and restart" (recompile True >> spawn "xmonad --restart"))
    subKeys "Actions" [ ("M-S-r", addName "Recompile and restart" (spawn "xmonad --restart"))
                      , ("M-C-l", addName "Lock screen" $ spawn locker)] ^++^
    subKeys "System" [ ("<XF86Sleep>", addName "Suspend machine" $ spawn "sudo pm-suspend")
                     , ("<XF86AudioRaiseVolume>", addName "Increase volume" $ spawn "amixer set Master 2%+")
                     , ("<XF86AudioLowerVolume>", addName "Decrease volume" $ spawn "amixer set Master 2%-")
                     , ("<XF86Favorites>", addName "Menu" $ treeselectAction def (systemMenu <> menu))
                     , ("M-S-<Space>", addName "Swap screens" swapNextScreen)
                     , ("M-<Backspace>", addName "Kill window" kill)
                     ]
                     -- scrot requires `unGrab`
                     -- , ("M-<Print>", addName "Take screenshot" $ spawn (application extraConfig "screenshot"))]
                     ^++^
    subKeys "Launchers" [ ("M-S-<Return>", addName "Open terminal" $ spawn $ terminal conf)
                        -- , ("M-n", (scratchSubmaps extraConfig) conf)
                        -- , ("M-s", searchSubmaps extraConfig conf)
                        -- , ("M-å", pomodoroSubmaps extraConfig conf)
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
                      -- These two keybindings might look redundant with the hjkl keybindings above,
                      -- but the directional bindings don't work in a tabbed layout. In the tabbed
                      -- layout, these are absolutely critical
                      , ("M-.", addName "Go to previous window" $ windows W.focusDown)
                      , ("M-,", addName "Go to next window" $ windows W.focusUp)
                      -- Keybindings for floating and unfloating
                      , ("M-S-.", addName "Sink floating to tiling" $ withFocused (windows . W.sink))
                      , ("M-S-,", addName "Float a window" $ withFocused (\w -> windows (W.float w (W.RationalRect (1/4) (1/4) (1/2) (1/2)))))
                      -- swap master
                      , ("M-S-m", addName "Swap master" $ windows W.swapMaster)
                      ] ^++^
    subKeys "Projects & Workspaces" (projectKeys topics xpconf conf) ^++^
    subKeys "Layout management" [ ("M-C-<Space>", addName "Toggle layout" $ sendMessage ToggleLayout)
                                , ("M-z", addName "Toggle zoom" $ sendMessage (Toggle "Zoom"))
                                , ("M-<Space>", addName "Next layout" $ sendMessage NextLayout)] ^++^
    subKeys "Resize" []
  where
    locker = "xset s activate"

