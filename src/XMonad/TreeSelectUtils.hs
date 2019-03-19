module TreeSelectUtils where

import XMonad.Actions.TreeSelect
import Data.Tree
import Data.List (sort, groupBy)
import Data.Function (on)
import qualified XMonad.StackSet as W
import XMonad.Core
import Control.Monad.State

-- fromWorkspace :: [String] -> Forest String
fromWorkspace = map go . group . sort
    where
        group = groupBy ((==) `on` (takeWhile (/= '.')))
        go (root:xs) = Node root [Node x [] | x <- xs]

treeselectWorkspaces' conf f = withWindowSet $ \w -> do
    let ws = map W.tag . W.hidden $ w
    treeselectWorkspace conf (fromWorkspace ws) f

