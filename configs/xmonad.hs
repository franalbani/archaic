
import XMonad hiding ((|||))
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Actions.CycleWS (prevWS, nextWS, prevScreen, nextScreen, shiftToPrev, shiftToNext)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Hooks.DynamicLog (ppTitle, ppOutput, dynamicLogWithPP, xmobarPP, xmobarColor, shorten)
import qualified XMonad.Prompt as XP
import XMonad.Actions.SpawnOn (shellPromptHere, spawnHere)
import XMonad.Prompt.AppendFile (appendFilePrompt)
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

-- Pantalla completa de Chromium:
import XMonad.Hooks.EwmhDesktops

-- 2018.03.18 necesario para evitar el problema
-- de la fuente por default inexistente (misc-fixed 12):
shellPromptConfig = XP.def {
                             XP.font = "xft:xos4 Terminus:pixelsize=12:antialias=true:hinting=true"
                           , XP.defaultText = ""
                           }

myManageHook  = manageHook defaultConfig <+>
                manageDocks <+>
                composeAll [isFullscreen --> doFullFloat] -- Para el fullscreen de Youtube

myTheme = XLD.def { decoHeight = 16
                  , activeColor = "#a6c292"
                  , activeBorderColor = "#a6c292"
                  , activeTextColor = "#000000"
                  , inactiveBorderColor = "#000000"
                  , XLD.fontName = "xft:xos4 Terminus:pixelsize=12:antialias=true:hinting=true"
                  }

myLayouts = avoidStruts $ (tabs ||| named "Grid" Grid)
    where
      tabs = smartBorders $ named "Tabs" (tabbed shrinkText myTheme)

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
            ((mod4Mask, xK_s), spawnLog "scrot '/tmp/%Y.%m.%d.%H.%M.%S.png' -e 'kolourpaint $f && thunar /tmp'"),

            -- 2015.11.02: para evitar cerrar X sin querer:
            ((mod4Mask .|. shiftMask, xK_q), spawnLog "nada"),

            -- 2015.11.15:
            ((mod4Mask, xK_w),  nextScreen),

            ((mod4Mask, xK_x), kill),
            ((mod4Mask, xK_p), shellPromptHere shellPromptConfig),
            ((mod4Mask, xK_b), sendMessage ToggleStruts)
            ]

main =	do
        -- 2015.10.10: Doble topbar:
        xmobarproc <- spawnPipe "bash -c \"tee >(xmobar -x 0 /etc/archaic_dots/xmobar/topbar) | xmobar -x 1 /etc/archaic_dots/xmobar/topbar\""
        xmonad $ docks defaultConfig
            {
            , workspaces = map show [1..9]
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
        	} `additionalKeys` myKeys
