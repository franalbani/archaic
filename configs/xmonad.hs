{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE FlexibleInstances #-}

import XMonad hiding ((|||))
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Actions.CycleWS (prevWS, nextWS, prevScreen, nextScreen, shiftToPrev, shiftToNext)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.DynamicLog (ppTitle, ppOutput, dynamicLogWithPP, xmobarPP, xmobarColor, shorten)
import qualified XMonad.Prompt as XP
import XMonad.Actions.SpawnOn (shellPromptHere, spawnHere)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

-- 2018.03.18 no logré importar sólo los que necesito:
-- import XMonad.Hooks.ManageDocks (docks, manageDocks, avoidStruts, ToggleStruts)
import XMonad.Hooks.ManageDocks

import qualified XMonad.Layout.Decoration as XLD
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.MultiColumns


-- Pantalla completa de Chromium:
import XMonad.Hooks.EwmhDesktops

-- ungrab
import XMonad.Util.Ungrab (unGrab)

-- 2022.12.27
-- xmonad-contexts
import qualified XMonad.Util.Dmenu       as D
import qualified XMonad.Actions.Contexts as C


-- 2018.03.18 necesario para evitar el problema
-- de la fuente por default inexistente (misc-fixed 12):
shellPromptConfig = XP.def {
                             XP.font = "xft:xos4 Terminus:pixelsize=12:antialias=true:hinting=true"
                           , XP.defaultText = ""
                           }

myManageHook  = manageHook def <+>
                manageDocks <+>
                composeAll [isFullscreen --> doFullFloat] -- Para el fullscreen de Youtube

myTheme = XLD.def { decoHeight = 16
                  , activeColor = "#a6c292"
                  , activeBorderColor = "#a6c292"
                  , activeTextColor = "#000000"
                  , inactiveBorderColor = "#000000"
                  , XLD.fontName = "xft:xos4 Terminus:pixelsize=12:antialias=true:hinting=true"
                  }

myLayouts = avoidStruts $ (tabs ||| named "Grid" Grid) ||| multicol
    where
      tabs = smartBorders $ named "Tabs" (tabbed shrinkText myTheme)
      multicol = multiCol [1] 1 0.01 (-0.5)

-- xmonad-contexts:
-- As a final step, which is required to make 
-- your layouts in non-visible contexts persistent
-- across restarts of XMonad, you have to make the following:
instance Read (Layout Window) where
    readsPrec _ = readsLayout (Layout myLayouts)


logDeco cmd = "(echo " ++ cmd ++ "; sleep 3) | /usr/bin/xmobar -o -t '}<fc=#ffff00>%StdinReader%</fc>{' -c '[Run StdinReader]'"
spawnLog cmd = do spawn $ logDeco cmd
                  spawnHere cmd

myKeys = [
            ((mod4Mask, xK_less),  prevWS),
            ((mod4Mask, xK_z), nextWS),

            ((mod4Mask .|. shiftMask, xK_period), shiftToNext),
            ((mod4Mask .|. shiftMask, xK_comma),  shiftToPrev),

            ((0, xK_Print ),  nextWS),
            ((shiftMask, xK_Print),   prevWS),

            ((mod4Mask, xK_Return), spawnLog "alacritty"),
            ((mod4Mask, xK_a), spawnLog "dbus-launch thunar"),
            ((mod4Mask, xK_i), spawnLog "mplayer -vf screenshot -fps 30 tv://"),
            ((mod4Mask, xK_s), unGrab *> spawnLog "scrot '/tmp/%Y.%m.%d.%H.%M.%S.png' -e 'kolourpaint $f && thunar /tmp'"),

            -- 2015.11.02: para evitar cerrar X sin querer:
            ((mod4Mask .|. shiftMask, xK_q), spawnLog "nada"),

            -- 2015.11.15:
            ((mod4Mask, xK_w),  nextScreen),
            -- 2020.09.27
            -- ((mod4Mask, xK_q),  prevScreen),

            ((mod4Mask, xK_x), kill),
            ((mod4Mask, xK_p), shellPromptHere shellPromptConfig),
            ((mod4Mask, xK_b), sendMessage ToggleStruts),
	    -- xmonad-contexts
	    ((mod4Mask, xK_c), C.listContextNames >>= D.dmenu >>= C.createAndSwitchContext),
	    ((mod4Mask .|. shiftMask, xK_c), C.listContextNames >>= D.dmenu >>= C.deleteContext >> return ())
            ]

myButtons = [((0, 9), const prevWS), ((0, 8), const nextWS)]

main =  do
        -- 2015.10.10: Doble topbar:
        xmobarproc <- spawnPipe "bash -c \"tee >(xmobar -x 0 /etc/archaic_dots/xmobar/topbar) | xmobar -x 1 /etc/archaic_dots/xmobar/topbar\""
        xmonad $ docks def
            {
              workspaces = map show [1..9]
            , modMask = mod4Mask
              -- Pantalla completa de Chromium:
            , handleEventHook = fullscreenEventHook
            , focusFollowsMouse = True
            , focusedBorderColor = "#018A8"
            , manageHook         = myManageHook
            , logHook            = dynamicLogWithPP $ xmobarPP
                                                        {   ppOutput = hPutStrLn xmobarproc,
                                                            ppTitle = xmobarColor "green" "" . shorten 80
                                                        }
            , layoutHook         = myLayouts
            } `additionalKeys` myKeys `additionalMouseBindings` myButtons
