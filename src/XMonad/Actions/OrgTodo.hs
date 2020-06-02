{-|
Module      : XMonad.Actions.OrgTodo
Description : Quickly add notes to orgmode inbox
Copyright   : (c) Mats Rauhala, 2020
Maintainer  : mats.rauhala@iki.fi
Stability   : experimental
Portability : POSIX

I'm using orgmode in a style where I add tasks to an "inbox" and then some
point later go through the inbox and sort it properly. This allows me to
quickly write down tasks that needs to be done, without changing context.
-}
module XMonad.Actions.OrgTodo where

import XMonad.Core
import XMonad.Prompt

import Data.Default (Default(..))

import Control.Monad.Trans (liftIO)

import Control.Lens (Lens', lens, (^.))

import Data.Time
       ( getCurrentTime
       , defaultTimeLocale
       , formatTime
       , getCurrentTimeZone
       , utcToLocalTime)

data OrgConfig
  = OrgConfig { orgConfigInbox :: FilePath
              , orgConfigXPConfig :: XPConfig
              }

inbox :: Lens' OrgConfig FilePath
inbox = lens orgConfigInbox (\a b -> a{orgConfigInbox=b})

xpconfig :: Lens' OrgConfig XPConfig
xpconfig = lens orgConfigXPConfig (\a b -> a{orgConfigXPConfig=b})

-- I know this is bad style, but most of xmonad-contrib uses default
instance Default OrgConfig where
  def = OrgConfig "/tmp/inbox.org" def

data OrgTodo = OrgTodo

instance XPrompt OrgTodo where
  showXPrompt _ = "TODO: "

addTodo :: OrgConfig -> X ()
addTodo conf = mkXPrompt OrgTodo (conf ^. xpconfig) complFun $ \todo -> liftIO $ do
  txt <- formatted todo <$> getCurrentTimeZone <*> getCurrentTime
  appendFile (conf ^. inbox) txt
  where
    formatted todo tz now = unlines ["* TODO " <> todo, formatTime defaultTimeLocale "  [%Y-%m-%d %a %H:%M]" (utcToLocalTime tz now)]
    complFun = const (pure [])
