module RestartFile (restartFile, resumeArgsFromFile, getArgs, withArgs) where

import XMonad.Core
import XMonad.Operations
import qualified XMonad.StackSet as W
import Graphics.X11.Xlib.Event
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import Data.Maybe
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import System.Environment (getArgs, withArgs)
import Control.Monad
import Control.Exception
import System.Posix.Process (executeFile)

stateFile = "xmonadargs.txt"

restartFile :: String -> Bool -> X ()
restartFile prog resume = do
    io $ appendFile "/home/masse/xmonad.log" $ "trying to restart"
    broadcastMessage ReleaseResources
    io . flush =<< asks display
    let wsData = show . W.mapLayout show . windowset
        maybeShow (t, Right (PersistentExtension ext)) = Just (t, show ext)
        maybeShow (t, Left str) = Just (t, str)
        maybeShow _ = Nothing
        extState = return . show . mapMaybe maybeShow . M.toList . extensibleState
    args <- if resume then return ["--resume"] else return []
    when resume $ do
      argsstr <- gets (\s ->  intercalate "\n" ("--resume":wsData s:extState s))
      catchIO $ writeFile stateFile argsstr
      return ()
    catchIO (executeFile prog True args Nothing)

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

resumeArgsFromFile :: IO [String]
resumeArgsFromFile = do
  let readLines = liftM lines . readFile $ stateFile
  args <- getArgs
  if ["--resume"] == args then
    catchAny readLines $ \e -> do
      appendFile "/home/masse/xmonad.log" $ "got error" ++ show e
      return args
  else return args
