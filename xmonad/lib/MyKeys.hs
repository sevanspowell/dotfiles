module MyKeys where

import XMonad

import XMonad.Actions.Navigation2D

import XMonad.Util.CustomKeys

myKeys = customKeys removedKeys addedKeys

removedKeys :: XConfig l -> [(KeyMask, KeySym)]
removedKeys _ = []

addedKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addedKeys conf @ (XConfig { XMonad.modMask = modMask }) = [
              -- Terminal
              ((modMask, xK_Return), spawn $ XMonad.terminal conf)

              -- Close window
            , ((modMask, xK_q), kill)

              -- Application launcher
            , ((modMask, xK_d), spawn "dmenu_run")

              -- Launch Emacs
            , ((modMask .|. shiftMask, xK_e), spawn "emacsclient -c -a emacs")

              -- Launch Internet Browser
            , ((modMask .|. shiftMask, xK_i), spawn "chromium")

              -- Restart xmonad
            , ((modMask, xK_r), restart "xmonad" True)

              -- Lock screen
            , ((modMask, xK_o), spawn "xscreensaver-command -lock")

              -- Screenshot 1
            , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")

              -- Screenshot 2
            , ((0, xK_Print), spawn "scrot")
            ]
