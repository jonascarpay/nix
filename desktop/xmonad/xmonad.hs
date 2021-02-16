{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad
import Data.Bool (bool)
import Data.Int (Int32)
import Control.Applicative
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (partition, findIndex, isPrefixOf)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Bool (bool)
import Data.Maybe (isJust)
import Numeric
import System.IO
import XMonad as XM
import XMonad.Config.Desktop as DC
import XMonad.Core
import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Spacing as SP
import qualified XMonad.StackSet as W
import qualified XMonad.Util.WorkspaceCompare as WC
import qualified XMonad.Actions.CycleRecentWS as CW
import qualified XMonad.Layout.LayoutModifier as LM
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import qualified XMonad.Util.Rectangle as UR

altMask = mod1Mask

ctrlMask = controlMask

-- termEmu = "termite -e /home/jmc/.nix-profile/bin/fish"
termEmu = "st"


data ToggleCenter w = TCActive | TCInactive
  deriving (Eq, Read, Show)

data CenterToggle = CenterToggle

instance Message CenterToggle
instance LM.LayoutModifier ToggleCenter Window where
  pureModifier _ _ Nothing layout = (layout, Nothing)
  pureModifier TCInactive _ _ layout = (layout, Nothing)
  pureModifier TCActive screen (Just stk) layout = 
    let (fg, bg) = partition ((== W.focus stk) . fst) layout
        fg' = fmap (\(wid, _) -> (wid, shrink screen)) fg
     in (fg' ++ bg, Nothing)

  pureMess tc msg = tgl tc <$> fromMessage msg
    where
      tgl TCActive CenterToggle = TCInactive
      tgl TCInactive CenterToggle = TCActive

shrink :: Rectangle -> Rectangle
shrink (Rectangle x y w h) = Rectangle (x+pad) (y+pad) (w - 2 * fromIntegral pad) (h - 2 * fromIntegral pad)
  where
    pad = round $ (fromIntegral w * 0.05 :: Double)

toggleCenter = LM.ModifiedLayout TCInactive

defaultSpacing, spacingDelta :: Int -- global constant to share value between reset key and layout
defaultSpacing = 60
spacingDelta = 15

myLayout = avoidStruts $ toggleCenter $ SP.spacingWithEdge defaultSpacing $ Tall 1 (3/100) (3/5)

data VResizeMsg = VShrink | VExpand
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
  xmonad
    $ ewmh
    $ dcfg dbus

cycleNonFocusDown :: W.Stack a -> W.Stack a
cycleNonFocusDown (W.Stack f us ds) = W.Stack f us' (reverse ds')
  where wins = us <> reverse ds
        wins' = let (h,t) = splitAt 1 wins
                 in t <> h
        (us',ds') = splitAt (length us) wins'

cycleNonFocusUp :: W.Stack a -> W.Stack a
cycleNonFocusUp (W.Stack f us ds) = W.Stack f (reverse us') ds'
  where wins = ds <> reverse us
        wins' = let (h,t) = splitAt 1 wins
                 in t <> h
        (ds',us') = splitAt (length ds) wins'

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

myCycleRecentWS :: [KeySym] -> KeySym -> KeySym -> X ()
myCycleRecentWS = CW.cycleWindowSets options
 where
   options w = map (flip W.view w) (recentTags w)
   recentTags w = W.tag <$> rotate (filterNonEmpty workspaces)
     where
       workspaces = W.workspaces w
       filterNonEmpty ws = case filter (isJust . W.stack) ws of
                             [] -> ws
                             ws' -> ws'
       rotate (x : xs) = xs ++ [x]

myKeys conf = M.fromList myKeyList <> keys desktopConfig conf
  where
    m = modMask conf
    myKeyList = 
      [ ((m, xK_f), spawn "firefox")
      , ((m, xK_d), withPwd $ maybe (spawn "dolphin") (\pwd -> spawn $ "dolphin " <> pwd))
      , ((m .|. shiftMask, xK_f), spawn "clipboard-firefox")
      , ((m, xK_Return), mkTerm "/home/jmc/.nix-profile/bin/fish")
      , ((m .|. ctrlMask, xK_Return), mkTerm "/home/jmc/.nix-profile/bin/tmux")
      , ((m, xK_z), sendMessage CenterToggle)
      , ((m .|. shiftMask, xK_Return), windows W.swapMaster)
      , ((m, xK_s), windows W.swapDown >> windows W.focusUp)
      , ((m, xK_c), spawn "emacsclient --create-frame --no-wait")
      -- , ((m, xK_c), spawn "emacs")
      , ((m .|. shiftMask, xK_h), sendMessage VShrink)
      , ((m .|. shiftMask, xK_l), sendMessage VExpand)
      , ((m, xK_grave), myCycleRecentWS [xK_Super_L] xK_grave xK_grave)
      , ((m, xK_y), windows $ W.modify' cycleNonFocusDown)
      , ((m .|. shiftMask, xK_y), windows $ W.modify' cycleNonFocusUp)
      -- Border control
      , ((m, xK_minus), SP.incScreenWindowSpacing $ fromIntegral spacingDelta)
      , ((m, xK_equal), SP.decScreenWindowSpacing $ fromIntegral spacingDelta)
      , ((m, xK_0), SP.setScreenWindowSpacing $ fromIntegral defaultSpacing)
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

mkTerm shell = withPwd $ maybe (runInTerm "" shell) (\cwd -> runInTerm ("-d" <> cwd) shell)

withPwd :: (Maybe FilePath -> X a) -> X a
withPwd f = withWindowSet $ \ws ->
  flip catchX (f Nothing) $ do
    Just xid <- pure $ W.peek ws
    (_ : _ : pid : _) <- words <$> runProcessWithInput "xprop" ["-id", show xid, "_NET_WM_PID"] ""
    (child : _) <- words <$> runProcessWithInput "ps" ["--ppid", pid, "-o", "pid="] ""
    (_ : cwd : _) <- words <$> runProcessWithInput "pwdx" [child] ""
    False <- pure $ "/proc/" `isPrefixOf` cwd
    f (Just cwd)

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
    greyOut = fg "#81a1c1"
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
