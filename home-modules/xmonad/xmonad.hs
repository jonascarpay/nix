{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (isPrefixOf, sortBy)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Bool (bool)
import Numeric
import System.IO
import XMonad
import XMonad.Config.Desktop as DC
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import qualified XMonad.Util.WorkspaceCompare as WC
import qualified XMonad.Actions.CycleWS as CW
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run

altMask = mod1Mask

ctrlMask = controlMask

termEmu = "termite -e /home/jmc/.nix-profile/bin/fish"

-- termEmu = "urxvt -e /home/jmc/.nix-profile/bin/fish"

newtype MyLayout a = MyLayout Rational
  deriving (Eq, Read, Show)

instance LayoutClass MyLayout a where
  description _ = "MyLayout"

  pureMessage (MyLayout w) m = fmap resize (fromMessage m)
    where resize Shrink = MyLayout (max 0 $ w-delta)
          resize Expand = MyLayout (min 1 $ w+delta)
          delta = 3 / 100

  pureLayout (MyLayout w) scr (W.Stack m [] []) = [(m, scr)]
  pureLayout (MyLayout w) scr (W.Stack m us ds) = 
    let (pm,pr) = splitHorizontallyBy w scr
        st = reverse us ++ ds
     in (m, pm) : zip st (splitVerticallyDiv (length st) (length us) 50 pr)

splitVerticallyDiv :: Int -> Int -> Int -> Rectangle -> [Rectangle]
splitVerticallyDiv n x r (Rectangle rx ry rw rh) = (\ry' -> Rectangle rx (fromIntegral ry') rw (fromIntegral rh')) <$> ys
  where
    rh' = div (fromIntegral rh - r) n
    ys = [ i*fromIntegral rh' + fromIntegral ry + bool 0 r (i >= x) | i <- take n [0..]]

main = do
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  forM [".xmonad-worspace-log", ".xmonad-title-log"] $ \file ->
    safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad
    $ ewmh
    $ dcfg dbus

dcfg dbus =
  desktopConfig
    { terminal = termEmu,
      modMask = mod4Mask,
      borderWidth = 0, -- Necessary to remove borders from floating windows
      focusFollowsMouse = False,
      logHook = myLogHook dbus,
      layoutHook = noBorders $ myLayout ||| Full,
      startupHook = startupHook desktopConfig,
      keys = myKeys,
      -- workspaces = show <$> [1..5],
      workspaces = [ "一", "二", "三", "四", "五" ], -- TODO maybe do this in post
      handleEventHook = handleEventHook def <+> fullscreenEventHook
    }

myLayout = avoidStruts $ spacingWithEdge 10 $ MyLayout (5/8)

myKeys conf = M.fromList
  [ ((m, xK_f), spawn "firefox")
  , ((m, xK_Return), mkTerm)
  , ((m, xK_o), spawn "rofi -show run")
  , ((m, xK_b), wal "haishoku" True False)
  , ((m, xK_c), spawn "emacsclient --create-frame --no-wait")
  , ((m .|. shiftMask, xK_b), wal "wal" True True)
  , ((m .|. ctrlMask, xK_1), wal "wal" False False)
  , ((m .|. ctrlMask, xK_2), wal "colorz" False False)
  , ((m .|. ctrlMask, xK_3), wal "haishoku" False False)
  , ((m .|. ctrlMask, xK_q), wal "wal" False True)
  , ((m .|. ctrlMask, xK_w), wal "colorz" False True)
  , ((m .|. ctrlMask, xK_e), wal "haishoku" False True)

  , ((m,               xK_n), CW.nextWS)
  , ((m,               xK_p), CW.prevWS)
  , ((m .|. shiftMask, xK_n), CW.shiftToNext >> CW.nextWS)
  , ((m .|. shiftMask, xK_p), CW.shiftToPrev >> CW.prevWS)
  , ((m, xK_grave), CW.moveTo CW.Next CW.NonEmptyWS)
  -- , ((m,               xK_Right), CW.nextScreen)
  -- , ((m,               xK_Left),  CW.prevScreen)
  -- , ((m .|. shiftMask, xK_Right), CW.shiftNextScreen)
  -- , ((m .|. shiftMask, xK_Left),  CW.shiftPrevScreen)
  ] <> keys desktopConfig conf
    where m = modMask conf

wal backend newpape light = do
  pape <-
    if newpape
      then return $ "/home/jmc/Wallpapers/papes/"
      else wrap "\"" "\"" <$> liftIO (readFile "/home/jmc/.cache/wal/wal")
  let cmd = unwords $ ["wal", "--backend", backend, "-i", pape] ++ (if light then ["-l"] else [])
  -- liftIO $ writeFile "/home/jmc/tmp" cmd
  spawn cmd

-- myBSP =
--   avoidStruts
--     $ spacingWithEdge 10
--     $ desktopLayoutModifiers
--     $ emptyBSP

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
    eventLogHook = def
      { ppOutput = dbusOutput dbus
      , ppLayout = const ""
      -- , ppSort = WC.getSortByXineramaRule
      }

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
