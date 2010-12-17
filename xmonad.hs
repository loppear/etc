import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Config.Gnome

import qualified XMonad.StackSet as W

import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutScreens
import XMonad.Layout.AutoMaster
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders


import XMonad.Prompt.Shell (shellPrompt, prompt, safePrompt)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Actions.Plane
import XMonad.Actions.Submap (submap)
import XMonad.Actions.Search (google, wikipedia,
                              promptSearch, selectSearch,
                              searchEngine)

import Data.Map as M (M.fromList, M.union, Map())


-- Layouts

myWorkspaces = ["dev", "net", "work", "web", "term", "comm", "spare",  "music" ]

layoutComm = avoidStruts $ ThreeCol 1 (3/100) (1/3) ||| Full

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
         , ((modm .|. shiftMask,   xK_p), layoutScreens 3 (Tall 1 (3/100) (1/2)))
         , ((modm .|. controlMask .|. shiftMask, xK_p), rescreen)

         , ((modm .|. shiftMask,   xK_c), kill)

         , ((modm .|. shiftMask,   xK_Return), spawn $ XMonad.terminal conf)
           -- Search keys
--         , ((modm,                 xK_s), selectSearch defaultXPConfig)

           -- Conkeror workaround, see http://conkeror.org/UpstreamBugs#FocusedpluginspreventConkerorkeybindingsfromworking
         , ((modm .|. shiftMask,  xK_f), spawn "conkeror -f unfocus")
         ]
           -- Plane
        ++ [ ((keyMask .|. modm, keySym), function (Lines 3) Finite direction) |
             (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft
           , (keyMask, function) <- [(0, planeMove), (shiftMask, planeShift)]
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
   , isFullscreen --> (doF W.focusDown <+> doFullFloat)
   , manageDocks
   ]


-- Search

pythondoc = searchEngine "pythondoc" "http://www.google.com/search?domains=docs.python.org&sitesearch=docs.python.org&sourceid=google-search&submit=submit&q="


searchMap method = M.fromList $
                   [ ((0, xK_g), method "firefox" google)
                   , ((0, xK_p), method "firefox" pythondoc)
                   , ((0, xK_w), method "firefox" wikipedia)
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
