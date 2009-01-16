import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace

import XMonad.Prompt.Shell (shellPrompt, prompt, safePrompt)
import XMonad.Prompt (defaultXPConfig)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.Search (google, wikipedia,
                              promptSearch, selectSearch,
                              searchEngine)

import Data.Map as M (M.fromList, M.union, Map())


-- Layouts

myWorkspaces = ["dev", "comm"] ++ map show [3..6]

layoutDev = avoidStruts $ ThreeCol 1 (3/100) (1/2) ||| Full

layoutComm = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myLayouts = onWorkspace "dev" layoutComm
            $ onWorkspace "comm" layoutComm
            $ onWorkspace "" layoutComm
            $ avoidStruts (layoutHook defaultConfig)


-- Keys

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
    M.fromList $
         [ ((modm,                 xK_space), shellPrompt defaultXPConfig)
         , ((modm .|. shiftMask,   xK_space ), sendMessage NextLayout)
         , ((modm .|. shiftMask,   xK_c), kill)

         , ((modm,                  xK_Return), spawn $ XMonad.terminal conf)
           -- Search keys
--         , ((modm,                 xK_s), submap $ searchMap $ promptSearch defaultXPConfig)
--         , ((modm .|. shiftMask,   xK_s), submap $ searchMap $ selectSearch)
         ]


-- Search

pythondoc = searchEngine "pythondoc" "http://www.google.com/search?domains=docs.python.org&sitesearch=docs.python.org&sourceid=google-search&submit=submit&q="


searchMap method = M.fromList $
                   [ ((0, xK_g), method "firefox" google)
                   , ((0, xK_p), method "firefox" pythondoc)
                   , ((0, xK_w), method "firefox" wikipedia)
                   ]


myTerminal = "urxvt -tr -tint grey -sh 40 -rv -fn 'xft:Bitstream Vera Sans Mono:pixelsize=12'"

-- Do it

main = xmonad defaultConfig
              { manageHook = manageDocks <+> manageHook defaultConfig
              , terminal   = myTerminal
              , logHook    = ewmhDesktopsLogHook
              , modMask    = mod3Mask
              , layoutHook = myLayouts
              , workspaces = myWorkspaces
              , keys = \c -> myKeys c `M.union` keys defaultConfig c
              }
