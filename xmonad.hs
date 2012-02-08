import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Config.Gnome

import qualified XMonad.StackSet as W

import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutScreens
import XMonad.Layout.AutoMaster
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import XMonad.Prompt.Shell (shellPrompt, prompt, safePrompt)
import XMonad.Prompt (defaultXPConfig)

import XMonad.Actions.CycleWS

import qualified Data.Map as M (fromList, union, Map())


-- Layouts

myWorkspaces = ["dev", "net", "work", "web", "term", "comm", "spare",  "music" ]

layoutComm = avoidStruts $ ThreeCol 1 (3/100) (1/3) ||| tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall 1 (3/100) (2/3)

layoutWeb = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall 1 (3/100) (1/2)

layoutDev = avoidStruts $ tiled ||| Mirror tiled ||| masterTiled ||| Full
  where
     tiled   = Tall nmaster delta tiledRatio
     masterTiled = autoMaster nmaster (3/100) tiled

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     tiledRatio   = 2/3

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myLayouts = onWorkspace "dev" layoutDev
            $ onWorkspace "comm" layoutComm
            $ onWorkspace "net" layoutWeb
            $ onWorkspace "work" layoutWeb
            $ onWorkspace "web" layoutWeb
            $ onWorkspace "music" layoutDev
            $ avoidStruts (layoutHook defaultConfig)


-- Keys

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
    M.fromList $         [ ((modm,                 xK_space), shellPrompt defaultXPConfig)
         , ((modm .|. shiftMask,   xK_space ), sendMessage NextLayout)
         , ((modm .|. shiftMask,   xK_p), layoutScreens 2 (TwoPane 0.5 0.5))
         , ((modm .|. controlMask .|. shiftMask, xK_p), rescreen)

         , ((modm .|. shiftMask,   xK_c), kill)

         , ((modm .|. shiftMask,   xK_Return), spawn $ XMonad.terminal conf)

           -- Conkeror workaround, see http://conkeror.org/UpstreamBugs#FocusedpluginspreventConkerorkeybindingsfromworking
         , ((modm .|. shiftMask,  xK_f), spawn "conkeror -f unfocus")
         ]
         -- a basic CycleWS setup
      ++ [
           ((modm,               xK_Down),  nextWS)
         , ((modm,               xK_Up),    prevWS)
         , ((modm .|. shiftMask, xK_Down),  shiftToNext >> nextWS)
         , ((modm .|. shiftMask, xK_Up),    shiftToPrev >> prevWS)
         , ((modm,               xK_Right), nextScreen)
         , ((modm,               xK_Left),  prevScreen)
         , ((modm .|. shiftMask, xK_Right), shiftNextScreen)
         , ((modm .|. shiftMask, xK_Left),  shiftPrevScreen)
         , ((modm,               xK_z),     toggleWS)
         ]

           -- Swap these to make sense
        ++ [
           -- Move focus to the next window
             ((modm,               xK_k     ), windows W.focusDown)
           -- Move focus to the previous window
           , ((modm,               xK_j     ), windows W.focusUp  )
           -- Swap the focused window with the next window
           , ((modm .|. shiftMask, xK_k     ), windows W.swapDown  )
           -- Swap the focused window with the previous window
           , ((modm .|. shiftMask, xK_j     ), windows W.swapUp    )
           ]
           -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
           -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
           --
        ++ [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
               | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
               , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myManageHook = composeAll
   [ className =? "Emacs"           --> doShift "dev"
   , className =? "Rhythmbox"       --> doShift "music"
   , className =? "Pidgin"          --> doShift "comm"
   , className =? "Empathy"         --> doShift "comm"
   , className =? "Unity-2d-panel"  --> doIgnore
   , isFullscreen --> (doF W.focusDown <+> doFullFloat)
   , manageDocks
   ]


myTerminal = "urxvt -tn xterm -tr -tint grey -sh 40 +sb -rv -fn 'xft:DejaVu Sans Mono:pixelsize=11'"

-- Do it

main = xmonad gnomeConfig
              { manageHook = myManageHook <+> manageHook gnomeConfig
              , terminal   = myTerminal
              , logHook    = dynamicLogWithPP defaultPP
                             >> ewmhDesktopsLogHook
                             >> setWMName "LG3D"

              , modMask    = mod4Mask
              , layoutHook = smartBorders (myLayouts)
              , workspaces = myWorkspaces
              , keys = \c -> myKeys c `M.union` keys gnomeConfig c
              }
