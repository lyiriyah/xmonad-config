--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import           System.Exit
import           System.IO
import           Text.Read
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Decoration
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt.Shell
import           XMonad.Util.Run
import           XMonad.Util.Themes
import           XMonad.Util.WorkspaceCompare


import qualified Data.Map                      as M
import qualified XMonad.StackSet               as W

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: [Char]
myTerminal = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: [Char]
myFocusedBorderColor :: [Char]
myNormalBorderColor = "#444444"
myFocusedBorderColor = "#076678"

myFullscreen :: X ()
myFullscreen = do
  sendMessage ToggleStruts
  sendMessage (Toggle "Full")

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modm }) =
  M.fromList
    $

    -- launch a terminal
       [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    -- , ((modm, xK_d), spawn "rofi -show drun")
       , ((modm, xK_d), shellPrompt def)

    -- close focused window
       , ((modm .|. shiftMask, xK_q), kill)

     -- Rotate through the available layout algorithms
       , ((modm, xK_space), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
       , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
       , ((modm, xK_n), refresh)

    -- Move focus to the next window
       , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the next window
       , ((modm, xK_j), focusDown)

    -- Move focus to the previous window
       , ((modm, xK_k), focusUp)

    -- Move focus to the master window
       , ((modm, xK_m), windows W.focusMaster)

    -- Swap the focused window and the master window
       , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
       , ((modm .|. shiftMask, xK_j), windows W.swapDown)

    -- Swap the focused window with the previous window
       , ((modm .|. shiftMask, xK_k), windows W.swapUp)

    -- Shrink the master area
       , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
       , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tiling
       , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
       , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
       , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
       , ((modm, xK_b), sendMessage ToggleStruts)

    -- Quit xmonad
       , ((modm .|. shiftMask, xK_c), io (exitWith ExitSuccess))

    -- Restart xmonad
       , ((modm, xK_c), spawn "xmonad --recompile; xmonad --restart")
       , ((modm .|. shiftMask, xK_f), myFullscreen)
       -- Pull from left
       , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
       -- Pull from right
       , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
       -- Pull from above
       , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
       -- Pull from below
       , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
       -- Merge all windows on workspace
       , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
       -- Unmerge all windows on workspace
       , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMergeAll))
       -- Unmerge singleton window on workspace
       , ( (modm .|. shiftMask .|. controlMask, xK_u)
         , withFocused (sendMessage . UnMerge)
         )
       -- Focus previous tab
       , ((modm .|. shiftMask, xK_h), onGroup W.focusUp')
       -- Focus next tab
       , ((modm .|. shiftMask, xK_l), onGroup W.focusDown')

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
       , ( (modm .|. shiftMask, xK_slash)
         , spawn ("echo \"" ++ help ++ "\" | xmessage -file -")
         )
       ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
       [ ((m .|. modm, k), windows $ onCurrentScreen f i)
       | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
       [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f  , m ) <- [(W.view, 0), (W.shift, shiftMask)]
       ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig { XMonad.modMask = modm }) =
  M.fromList
    $

    -- mod-button1, Set the window to floating mode and move by dragging
      [ ( (modm, button1)
        , (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
        )

    -- mod-button2, Raise the window to the top of the stack
      , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
      , ( (modm, button3)
        , (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
        )

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
      ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- myScr1Workspaces = map (marshall 1) myWorkspaces
mySpacing :: l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing = spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True

-- myLayout =
--   spacingRaw True (Border 4 4 4 4) True (Border 4 4 4 4) True $ toggleLayouts
--     (noBorders Full)
--     (modWorkspaces myScr1Workspaces Mirror $ lessBorders Screen tiled)
--  where
--      -- default tiling algorithm partitions the screen into two panes
--   tiled   = Tall nmaster delta ratio

--   -- The default number of windows in the master pane
--   nmaster = 1

--   -- Default proportion of screen occupied by master pane
--   ratio   = 1 / 2

--   -- Percent of screen to increment by when resizing panes
--   delta   = 3 / 100

myDecoTheme = def { activeColor         = "#222222"
                  , activeBorderColor   = "#222222"
                  , activeTextColor     = "#458488"
                  , inactiveColor       = "#222222"
                  , inactiveBorderColor = "#222222"
                  , inactiveTextColor   = "grey"
                  , urgentColor         = "red"
                  , urgentBorderColor   = "red"
                  , urgentTextColor     = "grey"
                  , fontName            = "xft:Hasklig:size=9"
                  , decoHeight          = 16
                  }

myLayout =
  windowNavigation
    $ addTabs shrinkText myDecoTheme
    $ subLayout [] Simplest
    $ boringWindows
    $ mySpacing
    $ toggleLayouts monocle
    $ onWorkspaces myScr1Workspaces (lessBorders Screen (Mirror centredmaster))
    $ lessBorders Screen centredmaster
 where
--  tiled            = Tall 1 (3 / 100) (1 / 2)
--  bstack           = Mirror tiled
  centredmaster    = ThreeColMid 1 (3 / 100) (1 / 2)
  monocle          = noBorders Full
  myScr1Workspaces = map (marshall 1) myWorkspaces

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
  [ className =? "MPlayer" --> doFloat
  , className =? "Gimp" --> doFloat
  , resource =? "desktop_window" --> doIgnore
  , resource =? "kdesktop" --> doIgnore
  ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

compareNumbers :: String -> String -> Ordering
compareNumbers a b =
  case (readMaybe a :: Maybe Int, readMaybe b :: Maybe Int) of
        -- if they're both valid numbers then compare them
    (Just x , Just y ) -> compare x y
    -- push numbers to the front of strings
    (Just _ , Nothing) -> LT
    (Nothing, Just _ ) -> GT
    -- strings get normal string comparison
    (Nothing, Nothing) -> compare a b

pp :: Handle -> PP
pp h = PP
  { ppCurrent          = xmobarColor "#458488" ""
  , ppVisible          = wrap "<" ">"
  , ppHidden           = id
  , ppHiddenNoWindows  = const ""
  , ppVisibleNoWindows = Nothing
  , ppUrgent           = xmobarColor "#FF0000" ""
  , ppSort             = mkWsSort $ return compareNumbers
  , ppSep              = " "
  , ppWsSep            = " "
  , ppTitle            = shorten 80
  , ppTitleSanitize    = xmobarStrip . dzenEscape
  , ppOrder            = id
  , ppExtras           = []
  , ppLayout           = (\x -> pad $ case x of
                           "Tabbed Spacing Tall"     -> "[]="
                           "Tabbed Spacing ThreeCol" -> "|M|"
                           "Tabbed Spacing Mirror ThreeCol" -> ">M>"
                           "Tabbed Spacing Full"     -> "[M]"
                           _                         -> x
                         )
  , ppOutput           = hPutStrLn h
  }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
  h  <- spawnPipe "xmobar -x 0 ~/.xmonad/xmobarrc"
  h1 <- spawnPipe "xmobar -x 1 ~/.xmonad/xmobarrc"
  xmonad $ docks def
    {
      -- simple stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = withScreens 2 myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

      -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

      -- hooks, layouts
    , layoutHook         = avoidStruts $ myLayout
    , manageHook         = insertPosition End Newer <+> fullscreenManageHook
    , handleEventHook    = fullscreenEventHook
    , logHook = let loggr screen handle =
                      dynamicLogWithPP . marshallPP screen . pp $ handle
                in  loggr 0 h >> loggr 1 h1
    , startupHook        = myStartupHook
    }

-- Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines
  [ "The default modifier key is 'alt'. Default keybindings:"
  , ""
  , "-- launching and killing programs"
  , "mod-Shift-Enter  Launch xterminal"
  , "mod-p            Launch dmenu"
  , "mod-Shift-p      Launch gmrun"
  , "mod-Shift-c      Close/kill the focused window"
  , "mod-Space        Rotate through the available layout algorithms"
  , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
  , "mod-n            Resize/refresh viewed windows to the correct size"
  , ""
  , "-- move focus up or down the window stack"
  , "mod-Tab        Move focus to the next window"
  , "mod-Shift-Tab  Move focus to the previous window"
  , "mod-j          Move focus to the next window"
  , "mod-k          Move focus to the previous window"
  , "mod-m          Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "mod-Return   Swap the focused window and the master window"
  , "mod-Shift-j  Swap the focused window with the next window"
  , "mod-Shift-k  Swap the focused window with the previous window"
  , ""
  , "-- resizing the master/slave ratio"
  , "mod-h  Shrink the master area"
  , "mod-l  Expand the master area"
  , ""
  , "-- floating layer support"
  , "mod-t  Push window back into tiling; unfloat and re-tile it"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "mod-comma  (mod-,)   Increment the number of windows in the master area"
  , "mod-period (mod-.)   Deincrement the number of windows in the master area"
  , ""
  , "-- quit, or restart"
  , "mod-Shift-q  Quit xmonad"
  , "mod-q        Restart xmonad"
  , "mod-[1..9]   Switch to workSpace N"
  , ""
  , "-- Workspaces & screens"
  , "mod-Shift-[1..9]   Move client to workspace N"
  , "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3"
  , "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3"
  , ""
  , "-- Mouse bindings: default actions bound to mouse events"
  , "mod-button1  Set the window to floating mode and move by dragging"
  , "mod-button2  Raise the window to the top of the stack"
  , "mod-button3  Set the window to floating mode and resize by dragging"
  ]
