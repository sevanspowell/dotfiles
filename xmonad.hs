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

import qualified Graphics.X11.Types as XT

-- TODO: Configure to use vim dir keys as window movement

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = insertPosition Below Newer <+> manageDocks <+> manageHook def
    , layoutHook = avoidStruts $ layoutHook def
    , handleEventHook = mconcat
                        [ docksEventHook
                        , handleEventHook def ]
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
    , modMask = mod4Mask -- Rebind mod to the super key
    , terminal = "urxvt"
    , focusedBorderColor = "#555"
    , normalBorderColor = "#333"
    , keys = myKeys 
    }
    `additionalKeys`
    [ ((mod4Mask, xK_o), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    , ((mod4Mask .|. shiftMask, xK_e), spawn "emacsclient -c -a emacs")
    ]

myKeys :: (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
myKeys conf@(XConfig {XMonad.modMask = modMask}) =
    let custom = M.fromList $ [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
               , ((modMask, xK_q), kill)
               , ((modMask, xK_d), spawn "dmenu_run")
               , ((modMask, xK_r), restart "xmonad" True)
               ]
    in custom <> keys def conf
