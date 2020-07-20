-- peek

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad
import qualified DBus as D
import qualified DBus.Client as D
import Data.Function (on)
import Data.List (isPrefixOf, sortBy)
import Data.Map (fromList)
import Data.Monoid ((<>))
import Numeric
import System.IO
import System.IO.Error (catchIOError)
import System.Process (readProcess)
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Config.Desktop as DC
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import qualified XMonad.Layout.Hidden as LH
import qualified XMonad.Layout.IndependentScreens as IS
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import qualified XMonad.Util.WorkspaceCompare as WC

modm = mod4Mask

altMask = mod1Mask

ctrlMask = controlMask

termEmu = "termite -e /home/jmc/.nix-profile/bin/fish"

-- termEmu = "urxvt -e /home/jmc/.nix-profile/bin/fish"

main = do
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  forM [".xmonad-worspace-log", ".xmonad-title-log"] $ \file ->
    safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad $ ewmh
    $ navigation2D
      navcfg
      (xK_k, xK_h, xK_j, xK_l)
      [ (modm, windowGo),
        (modm .|. shiftMask, windowSwap)
      ]
      True
    $ flip additionalKeys (applicationKeys ++ navigationKeys)
    $ dcfg dbus

dcfg dbus =
  desktopConfig
    { terminal = termEmu,
      modMask = modm,
      borderWidth = 0, -- Necessary to remove borders from floating windows
      focusFollowsMouse = False,
      logHook = myLogHook dbus,
      keys = keyBindings,
      layoutHook = LH.hiddenWindows . noBorders $ myBSP ||| simpleTabbed,
      startupHook = startupHook desktopConfig,
      workspaces = IS.withScreens 2 [show n | n <- [1 .. 5]],
      handleEventHook = handleEventHook def <+> fullscreenEventHook
    }

keyBindings conf =
  let m = modMask conf
   in fromList
        [ ((m .|. e, k), windows (IS.onCurrentScreen f ws))
          | (k, ws) <- zip [xK_1 .. xK_9] (IS.workspaces' conf),
            (e, f) <- [(0, W.view), (shiftMask, W.shift)]
        ]
        <> keys desktopConfig conf

applicationKeys =
  [ ((modm, xK_f), spawn "firefox"),
    -- , ((modm, xK_Return), spawn termEmu)
    ((modm, xK_Return), mkTerm),
    ((modm, xK_o), spawn "rofi -show run"),
    ((modm, xK_b), wal "haishoku" True False),
    ((modm .|. shiftMask, xK_b), wal "wal" True True),
    ((modm .|. ctrlMask, xK_1), wal "wal" False False),
    ((modm .|. ctrlMask, xK_2), wal "colorz" False False),
    ((modm .|. ctrlMask, xK_3), wal "haishoku" False False),
    ((modm .|. ctrlMask, xK_q), wal "wal" False True),
    ((modm .|. ctrlMask, xK_w), wal "colorz" False True),
    ((modm .|. ctrlMask, xK_e), wal "haishoku" False True)
  ]

navigationKeys =
  [ ((modm, xK_r), sendMessage Rotate),
    ((modm, xK_s), sendMessage Swap),
    ((modm .|. altMask, xK_l), sendMessage $ ExpandTowards R),
    ((modm .|. altMask, xK_h), sendMessage $ ExpandTowards L),
    ((modm .|. altMask, xK_j), sendMessage $ ExpandTowards D),
    ((modm .|. altMask, xK_k), sendMessage $ ExpandTowards U),
    ((modm .|. altMask .|. ctrlMask, xK_l), sendMessage $ ShrinkFrom R),
    ((modm .|. altMask .|. ctrlMask, xK_h), sendMessage $ ShrinkFrom L),
    ((modm .|. altMask .|. ctrlMask, xK_j), sendMessage $ ShrinkFrom D),
    ((modm .|. altMask .|. ctrlMask, xK_k), sendMessage $ ShrinkFrom U),
    ((modm, xK_r), sendMessage Rotate),
    ((modm, xK_s), sendMessage Swap),
    ((modm, xK_n), sendMessage FocusParent),
    ((modm .|. ctrlMask, xK_n), sendMessage SelectNode),
    ((modm .|. shiftMask, xK_n), sendMessage MoveNode),
    ((modm, xK_a), sendMessage Equalize),
    ((modm .|. shiftMask, xK_a), sendMessage Balance),
    ((modm, xK_m), withFocused LH.hideWindow),
    ((modm .|. shiftMask, xK_m), LH.popNewestHiddenWindow)
    -- , ((modm .|. shiftMask, xK_t), toggleWindowSpacingEnabled >> toggleSmartSpacing >> toggleScreenSpacingEnabled)
  ]

navcfg =
  def
    { defaultTiledNavigation = centerNavigation,
      unmappedWindowRect = [("Full", singleWindowRect)] -- Needed for full-screen hjkl-navigation
    }

wal backend newpape light = do
  pape <-
    if newpape
      then return $ "/home/jmc/Wallpapers/papes/"
      else wrap "\"" "\"" <$> liftIO (readFile "/home/jmc/.cache/wal/wal")
  let cmd = unwords $ ["wal", "--backend", backend, "-i", pape] ++ (if light then ["-l"] else [])
  liftIO $ writeFile "/home/jmc/tmp" cmd
  spawn cmd

myBSP =
  avoidStruts
    $ borderResize
    $ spacingWithEdge 10
    $ desktopLayoutModifiers
    $ emptyBSP

mkTerm = withWindowSet launchTerminal
  where
    shell = "/home/jmc/.nix-profile/bin/fish"
    launchTerminal ws =
      flip catchX (runInTerm "" shell) $ do
        Just xid <- pure $ W.peek ws
        (_ : _ : pid : _) <- words <$> runProcessWithInput "xprop" ["-id", show xid, "_NET_WM_PID"] ""
        (child : _) <- words <$> runProcessWithInput "ps" ["--ppid", pid, "-o", "pid="] ""
        (_ : cwd : _) <- words <$> runProcessWithInput "pwdx" [child] ""
        False <- pure $ "/proc/" `isPrefixOf` cwd
        runInTerm ("-d " <> cwd) shell

myLogHook :: D.Client -> X ()
myLogHook dbus = do
  -- hiddens <- AM.withMinimized (pure . length)
  dynamicLogWithPP eventLogHook
  where
    eventLogHook =
      def
        { ppOutput = dbusOutput dbus,
          ppCurrent =
            wrap "%{R}%{T2} " " %{T-}%{R}"
              -- . (if hiddens > 0 then (<> ('+' : show hiddens)) else id)
              . parseWS,
          ppVisible = wrap " " " " . parseWS,
          ppUrgent = wrap " " " " . parseWS,
          ppHidden = wrap " " " " . parseWS,
          ppLayout = const "",
          ppWsSep = "",
          ppSort = WC.mkWsSort WC.getWsCompareByTag,
          ppSep = "  ",
          ppTitle = shorten 300
        }
    parseWS :: String -> String
    parseWS (b : '_' : d) =
      let d' = read (b : [])
          b' = read d
          i = 2 * (b' - 1) + d'
          letter = if d' == 0 then 'L' else 'R'
          -- wrap str = "%{A1:wmctrl -s " ++ show i ++ ":}" ++ str ++ "%{A}"
          wrap = id
       in wrap (letter : d)
    parseWS str = str

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
