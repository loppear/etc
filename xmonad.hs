import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Config.Gnome

import qualified XMonad.StackSet as W

import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace

import XMonad.Prompt.Shell (shellPrompt, prompt, safePrompt)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Actions.Plane
import XMonad.Actions.Submap (submap)
import XMonad.Actions.Search (google, wikipedia,
                              promptSearch, selectSearch,
                              searchEngine)

import Data.Map as M (M.fromList, M.union, Map())


-- Layouts

myWorkspaces = ["dev", "net", "web", "music", "term", "comm", "spare" ] -- ++ map show [3..6]

layoutComm = avoidStruts $ ThreeCol 1 (3/100) (1/2) ||| Full

layoutDev = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myLayouts = onWorkspace "dev" layoutDev
            $ onWorkspace "comm" layoutComm
            $ onWorkspace "net" layoutDev
            $ avoidStruts (layoutHook defaultConfig)


-- Keys

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
    M.fromList $
         [ ((modm,                 xK_space), shellPrompt defaultXPConfig)
         , ((modm .|. shiftMask,   xK_space ), sendMessage NextLayout)
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
           ]



-- Search

pythondoc = searchEngine "pythondoc" "http://www.google.com/search?domains=docs.python.org&sitesearch=docs.python.org&sourceid=google-search&submit=submit&q="


searchMap method = M.fromList $
                   [ ((0, xK_g), method "firefox" google)
                   , ((0, xK_p), method "firefox" pythondoc)
                   , ((0, xK_w), method "firefox" wikipedia)
                   ]

myTerminal = "urxvt -tr -tint grey -sh 40 +sb -rv -fn 'xft:Bitstream Vera Sans Mono:pixelsize=11'"

-- Do it

main = xmonad gnomeConfig
              { manageHook = manageDocks <+> manageHook gnomeConfig
              , terminal   = myTerminal
              , logHook    = dynamicLogWithPP defaultPP
                             >> ewmhDesktopsLogHook
                             >> setWMName "LG3D"

              , modMask    = mod4Mask
              , layoutHook = myLayouts
              , workspaces = myWorkspaces
              , keys = \c -> myKeys c `M.union` keys gnomeConfig c
              }
