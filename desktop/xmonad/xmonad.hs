{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad
import Data.Bool (bool)
import Control.Applicative
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (isPrefixOf, sortBy)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Bool (bool)
import Numeric
import System.IO
import XMonad as XM
import XMonad.Config.Desktop as DC
import XMonad.Core
import XMonad.Hooks.DynamicLog as DL
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
import qualified XMonad.Util.Rectangle as UR

altMask = mod1Mask

ctrlMask = controlMask

termEmu = "termite -e /home/jmc/.nix-profile/bin/fish"

-- termEmu = "urxvt -e /home/jmc/.nix-profile/bin/fish"

data TallDock a = TallDock
  { tdNMain :: !Int
  , tdRatioDock :: !Rational
  , tdRatioSide :: !Rational
  , tdZoomSide :: Bool
  }
  deriving (Show, Read)

data ZoomState

instance LayoutClass TallDock a where
  description _ = "TallDock"

  pureLayout (TallDock m rd rs zoom) rect (W.Stack x pre post) =
      (x, bool winX (interpolateRect 0.7 winX (pad (-15) rect)) zoom)
        : zip (reverse pre) winsPre <> zip post winsPost
    where
      n = length pre + length post + 1
      (winsPre, winX : winsPost) = splitAt (length pre) rects

      rects
        | m == 0 = splitVertically n rect
        | m >= n = tileDock n rect
        | otherwise = let (l,r) = splitHorizontallyBy rs rect
                       in tileDock m l <> splitVertically (n - m) r

      tileDock 0 _ = []
      tileDock 1 r = [r]
      tileDock n r = top : bottoms
        where (top, bottom) = splitVerticallyBy rd r
              bottoms = splitHorizontally (n - 1) bottom

      pad :: Integer -> Rectangle -> Rectangle
      pad x = UR.withBorder x x x x 0

      interpolateRect :: Double -> Rectangle -> Rectangle -> Rectangle
      interpolateRect r (Rectangle x y w h) (Rectangle x' y' w' h') =
          Rectangle (x <-> x') (y <-> y') (w <-> w') (h <-> h')
        where
          (<->) :: Integral a => a -> a -> a
          a <-> b = round $ realToFrac a * (1-r) + realToFrac b * r

  pureMessage (TallDock m rd rs z) msg
      =   fmap resize (fromMessage msg)
      <|> fmap vresize (fromMessage msg)
      <|> fmap incDock (fromMessage msg)
      <|> fmap togglezoom (fromMessage msg)

    where
      delta = 3 / 100
      resize Shrink = TallDock m rd (max 0.1 $ rs-delta) False
      resize Expand = TallDock m rd (min 0.9 $ rs+delta) False
      vresize VShrink = TallDock m (max 0.1 $ rd-delta) rs False
      vresize VExpand = TallDock m (min 0.9 $ rd+delta) rs False
      incDock (IncMasterN d) = TallDock (max 0 (m+d)) rd rs False
      togglezoom ZoomToggle = TallDock m rd rs (not z)
      togglezoom ZoomUnzoom = TallDock m rd rs False

defaultTallDock :: TallDock a
defaultTallDock = TallDock 1 (3/4) (1/2) False

myLayout = avoidStruts $ spacingWithEdge 10 $ defaultTallDock
-- myLayout = avoidStruts $ defaultTallDock

data VResizeMsg = VShrink | VExpand
  deriving (Eq, Show, Typeable, Message)

data ZoomMsg = ZoomToggle | ZoomUnzoom
  deriving (Eq, Show, Typeable, Message)

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
  -- forM [".xmonad-worspace-log", ".xmonad-title-log"] $ \file ->
  --   safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad
    $ ewmh
    $ dcfg dbus

dcfg dbus =
  desktopConfig
    { terminal = termEmu,
      modMask = mod4Mask,
      borderWidth = 0, -- Necessary to remove borders from floating windows
      focusFollowsMouse = False,
      logHook = polybarLog dbus,
      layoutHook = myLayout ||| Full,
      -- startupHook = startupHook desktopConfig,
      keys = myKeys,
      workspaces = show <$> [1..5],
      handleEventHook = handleEventHook def <+> fullscreenEventHook
    }

myKeys conf = handlezoom $ M.fromList myKeyList <> keys desktopConfig conf
  where
    handlezoom = M.insert (m, xK_z) (sendMessage ZoomToggle) -- . fmap (>> sendMessage ZoomUnzoom)
    m = modMask conf
    myKeyList = 
      [ ((m, xK_f), spawn "firefox")
      , ((m, xK_Return), mkTerm)
      , ((m .|. shiftMask, xK_Return), windows W.swapMaster)
      , ((m, xK_s), windows W.swapDown >> windows W.focusUp)
      , ((m, xK_b), wal "haishoku" True False)
      , ((m, xK_c), spawn "emacsclient --create-frame --no-wait")
      -- , ((m, xK_c), spawn "emacs")
      , ((m .|. shiftMask, xK_b), wal "wal" True True)
      , ((m .|. shiftMask .|. ctrlMask, xK_1), wal "wal" False False)
      , ((m .|. shiftMask .|. ctrlMask, xK_2), wal "colorz" False False)
      , ((m .|. shiftMask .|. ctrlMask, xK_3), wal "haishoku" False False)
      , ((m .|. shiftMask .|. ctrlMask, xK_q), wal "wal" False True)
      , ((m .|. shiftMask .|. ctrlMask, xK_w), wal "colorz" False True)
      , ((m .|. shiftMask .|. ctrlMask, xK_e), wal "haishoku" False True)
    
      , ((m .|. shiftMask, xK_h), sendMessage VShrink)
      , ((m .|. shiftMask, xK_l), sendMessage VExpand)
      , ((m, xK_n), CW.moveTo CW.Next CW.HiddenWS)
      , ((m, xK_p), CW.moveTo CW.Prev CW.HiddenWS)
      , ((m .|. controlMask, xK_n), CW.shiftTo CW.Next CW.HiddenWS)
      , ((m .|. controlMask, xK_p), CW.shiftTo CW.Prev CW.HiddenWS)
      , ((m .|. shiftMask, xK_n), CW.shiftTo CW.Next CW.HiddenWS >> CW.moveTo CW.Next CW.HiddenNonEmptyWS)
      , ((m .|. shiftMask, xK_p), CW.shiftTo CW.Prev CW.HiddenWS >> CW.moveTo CW.Prev CW.HiddenNonEmptyWS)
      , ((m, xK_grave), CW.moveTo CW.Next CW.HiddenNonEmptyWS)
      ] <>
      [((m .|. mask, k), windows f)
            | (i, k) <- zip (XM.workspaces conf) [xK_1 .. xK_9]
            , (mask, f) <- [ (0, W.greedyView i)
                           , (shiftMask, W.shift i)
                           , (controlMask, W.greedyView i . W.shift i)
                           ]
      ] <>
      [((m .|. mask, key), screenWorkspace scr >>= flip whenJust (windows . f))
          | (key, scr) <- zip [xK_w, xK_e, xK_r] [0..]
          -- | (key, scr) <- zip [xK_e, xK_w] [0..]
          , (mask, f) <- [ (0, W.view)
                         , (shiftMask, \n -> W.shift n)
                         , (controlMask, \n -> W.view n . W.shift n) -- TODO greedyview vs view?
                         ]
      ]

wal backend newpape light = do
  pape <-
    if newpape
      then return $ "/home/jmc/Wallpapers/papes/"
      else wrap "\"" "\"" <$> liftIO (readFile "/home/jmc/.cache/wal/wal")
  let cmd = unwords $ ["wal", "--backend", backend, "-i", pape] ++ (if light then ["-l"] else [])
  -- liftIO $ writeFile "/home/jmc/tmp" cmd
  spawn cmd

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

polybarLog :: D.Client -> X ()
polybarLog dbus = do
  dynamicLogWithPP eventLogHook
  where
    eventLogHook = def
      { ppOutput = dbusOutput dbus
      , ppLayout = const ""
      , ppCurrent          = rev . base
      , ppVisible          = ul' . base
      , ppVisibleNoWindows = Just (ul' . greyOut . base)
      , ppHidden           = base
      , ppHiddenNoWindows  = greyOut . base
      , ppWsSep = ""
      , ppSep = "   "
      , ppTitle = font2 . DL.shorten 80
      }
    base = DL.pad . weebify
    rev = DL.wrap "%{R}" "%{R-}"
    greyOut = fg "#444444"
    ul hex = DL.wrap ("%{u" <> hex <> "}%{+u}") "%{-u}"
    ul' = DL.wrap "%{+u}" "%{-u}"
    fg hex = DL.wrap ("%{F" <> hex <> "}") "%{F-}"
    bg hex = DL.wrap ("%{B" <> hex <> "}") "%{B-}"
    font2 = DL.wrap "%{T2}" "%{T-}"

    weebify "1" = "壱" -- "一"
    weebify "2" = "弐" -- "二"
    weebify "3" = "参" -- "三"
    weebify "4" = "四"
    weebify "5" = "五"
    weebify "6" = "六"
    weebify "7" = "七"
    weebify "8" = "八"
    weebify "9" = "九"
    weebify str = str

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
