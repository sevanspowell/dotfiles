import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Prompt
import System.IO
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Actions.Navigation2D

import qualified Graphics.X11.Types as XT

import MyKeys

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad =<< xmobar myConfig

backgroundColor = "#000000"
middleColor     = "#343434"
foregroundColor = "#555555"

myConfig = def
    { borderWidth = 1
    , manageHook = insertPosition Below Newer <+> manageDocks <+> manageHook def
    , layoutHook = avoidStruts $ spacingWithEdge 2 emptyBSP ||| spacingWithEdge 2 (ThreeCol 1 (3/100) (-1/3)) ||| spacingWithEdge 2 Full
    , handleEventHook = mconcat [ docksEventHook , handleEventHook def ]
    , logHook = dynamicLogWithPP xmobarPP { ppTitle = xmobarColor "green" "" . shorten 50 }
    , modMask = mod4Mask
    , terminal = "urxvt"
    , focusedBorderColor = foregroundColor
    , normalBorderColor = middleColor
    , keys = myKeys
    }
