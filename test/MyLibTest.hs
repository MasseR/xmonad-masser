{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Test.Hspec
import XMonad.Config.MasseR.ExtraConfig
import qualified Dhall

main :: IO ()
main = hspec spec

readDhallConfig :: FilePath -> IO Config
readDhallConfig = Dhall.inputFile Dhall.auto

foldBindings :: SubCommandF a [String] -> [String]
foldBindings = \case
  ActF { subCommandPrefixF, subCommandNameF, subCommandCommandF } -> [subCommandPrefixF, subCommandNameF, show subCommandCommandF]
  SubMapF { subCommandPrefixF, subCommandNameF, subCommandSubF } -> concat $ [subCommandPrefixF, subCommandNameF] : subCommandSubF

foldTree :: TreeF [String] -> [String]
foldTree = \case
  NodeF { treeNameF, treeExtraF, treeValueF, treeChildrenF } -> treeNameF : treeExtraF : show treeValueF : concat treeChildrenF

spec :: Spec
spec = describe "Reading the dhall configuration" $ do
  it "Reads the simplest configuration" $ do
    c <- readDhallConfig "test/data/simple.dhall"
    pure ()
  it "Reads some bindings" $ do
    c <- readDhallConfig "test/data/bindings.dhall"
    let bindings = configBindings c
    map (fold foldBindings) bindings `shouldBe` [["M-m","music","M-x","asd","Spawn \"foo\""]]
  it "Reads some menus" $ do
    c <- readDhallConfig "test/data/menu.dhall"
    let menu = configMenu c
    map (fold foldTree) menu `shouldBe` [["terminal", "", "Just (Spawn \"asd\")"]]
  it "Reads a recursive menus" $ do
    c <- readDhallConfig "test/data/recursive_menu.dhall"
    let menu = configMenu c
    map (fold foldTree) menu `shouldBe` [["music","","Nothing","Play previous","","Just (Spawn \"sp prev\")","Play next","","Just (Spawn \"sp next\")"]]

