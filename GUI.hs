{-# LANGUAGE OverloadedStrings #-}
module GUI (gui) where

import Control.Monad
import Graphics.UI.WX (Prop(..))
import ListUtils (sortOn)
import Parser (CostCentre(..), CostCentreData(..), TimeAlloc(..))
import TextUtils (showText)
import qualified Data.Text as Text
import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore as WXC

treeAppendText :: WXC.TreeCtrl a -> WXC.TreeItem -> String -> IO WXC.TreeItem
treeAppendText tree node text = WXC.treeCtrlAppendItem tree node text (-1) (-1) WX.objectNull

addCostCentres :: WXC.TreeCtrl a -> WXC.TreeItem -> [CostCentre] -> IO ()
addCostCentres tree node ccs =
  forM_ (sortOn (negate . timePercent . ccInherited . ccData) ccs) $ \cc -> do
    let
      cd = ccData cc
      text = Text.unwords
        [ Text.concat [ccModule cd, ".", ccName cd]
        , showText (timePercent (ccInherited cd))
        , Text.concat ["(", showText (timePercent (ccIndividual cd)), ")"]
        ]
    item <- treeAppendText tree node $ Text.unpack text
    addCostCentres tree item $ ccChildren cc

gui :: [CostCentre] -> IO ()
gui costCentres = do
  f <- WX.frame [WX.text := "Profile analyzer"]
  tree <- WX.treeCtrl f [] -- [WX.color := WX.rgb 255 255 255, WX.bgcolor := WX.rgb 0 0 0]
  root <- WXC.treeCtrlAddRoot tree "Cost centres" (-1) (-1) WX.objectNull
  addCostCentres tree root costCentres
  WXC.treeCtrlExpand tree root
  return ()
