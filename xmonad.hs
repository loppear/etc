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
                              simpleEngine)

import Data.Map as M (M.fromList, M.union, Map())


-- Layouts

myWorkspaces = ["dev", "comm"] ++ map show [3..6]

layoutDev = avoidStruts $ ThreeCol 1 (3/100) (1/2) ||| Full

layoutComm = avoidStruts $ tiled ||| Mirror tiled ||| Full
    where
      tiled = Tall 1 (3/100) (2/3)

myLayouts = onWorkspace "dev" layoutDev
            $ onWorkspace "comm" layoutComm
            $ avoidStruts (layoutHook defaultConfig)


-- Keys

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = modm}) =
    M.fromList $
         [ ((modm,                 xK_x), shellPrompt defaultXPConfig)
           
           -- Search keys
         , ((modm,                 xK_s), submap $ searchMap $ promptSearch defaultXPConfig)
         , ((modm .|. shiftMask,   xK_s), submap $ searchMap $ selectSearch)
         ]


-- Search

pythondoc = simpleEngine "http://www.google.com/search?domains=docs.python.org&sitesearch=docs.python.org&sourceid=google-search&submit=submit&q="


searchMap method = M.fromList $
                   [ ((0, xK_g), method "firefox" google)
                   , ((0, xK_p), method "firefox" pythondoc)
                   , ((0, xK_w), method "firefox" wikipedia)
                   ]


myTerminal = "urxvt -tr -tint grey -sh 40 -rv -fn 'xft:Bitstream Vera Sans Mono:pixelsize=14'"

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

