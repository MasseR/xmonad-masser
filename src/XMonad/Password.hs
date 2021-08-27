module XMonad.Password (passGeneratePrompt, passPrompt) where

import           Control.Monad.Trans   (liftIO)
import           Data.Function         (on)
import           Data.List             (foldl', scanl', sort, sortBy)
import           System.Directory      (getHomeDirectory)
import           System.FilePath.Posix (dropExtension, takeExtension, (</>))
import           System.Posix.Env      (getEnv)
import           XMonad.Core
import           XMonad.Prompt
import           XMonad.Util.Run       (runProcessWithInput)
import Data.Char (toLower)

newtype Pass = Pass { passLabel :: String }

-- Rosetta code levenshtein
levenshtein :: String -> String -> Int
levenshtein s1 s2 = last $ foldl' transform [0..length s1] s2
  where
    transform [] _ = []
    transform ns@(n:ns1) c = scanl' calc (n+1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y+1, z+1, x + (fromEnum (c1 /= c) * 2)]

instance XPrompt Pass where
  showXPrompt p = passLabel p <> ": "
  commandToComplete _ = id
  nextCompletion _ = getNextCompletion

passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt _ = return () -- Not implemented

passPrompt :: XPConfig -> X ()
passPrompt = mkPassPrompt "Select password" selectPassword

mkPassPrompt :: String -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt label f conf = do
  -- I'm just sorting here, but could use some kind of fuzzy matching instead, but it requires a bit more effort
  passwords <- sort <$> liftIO getPasswords
  -- Other change, use infixof instead of prefixof
  mkXPrompt (Pass label) conf (\input -> pure (sortBy (compare `on` levenshtein input) . filter (consumes (toLowerCase input) . toLowerCase) $ passwords)) f
  where
    toLowerCase = map toLower
    consumes [] _ = True -- everything consumed
    consumes (_:_) [] = False -- all not consumed
    consumes (a:xs) (a':ys) | a == a' = consumes xs ys
                            | otherwise = consumes (a:xs) ys
    getStore = do
      let storeDefault = (</> ".password-store")
      maybe (storeDefault <$> getHomeDirectory) pure =<< getEnv "PASSWORD_STORE_DIR"
    getPasswords = do
      passwordStoreDir <- getStore
      files <- runProcessWithInput "find" [ passwordStoreDir, "-type", "f", "-name", "*.gpg", "-printf", "%P\n"] []
      return . map (\path -> if path `hasExtension` ".gpg" then dropExtension path else path) . lines $ files
    hasExtension path ext = takeExtension path == ext


selectPassword :: String -> X ()
selectPassword pass = spawn $ "PASSWORD_STORE_CLIP_TIME=10 pass --clip " ++ pass
