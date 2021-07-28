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



data TallAccordion a = TallAccordion !Rational !Rational
  deriving (Eq, Show, Read)

instance XM.LayoutClass TallAccordion a where
  pureLayout (TallAccordion rm rs) screen stk@(W.Stack cur pre post) =
      case stk of 
        (W.Stack _ [] []) -> [(cur, screen)]
        (W.Stack _ [wmain] []) -> [(cur, side), (wmain, prim)]
        (W.Stack _ [] [wside]) -> [(cur, prim), (wside, side)]
        _ -> (cur, rcur) : zip (reverse pre) rpre <> zip post rpost
    where
      n = length pre + length post + 1

      (prim,side) = splitHorizontallyBy rm screen
      (sec, rest) = splitVertically (n-2) <$> splitVerticallyBy rs side

      rects = prim : sec : rest
      (rpre, rcur : rpost) = splitAt (length pre) rects

      interleave [] bs = bs
      interleave (a:as) bs = a : interleave bs as

  pureMessage (TallAccordion rm rs) msg = fmap mainResize (fromMessage msg) <|> fmap sideResize (fromMessage msg)
    where
      delta = 3/100
      mainResize Expand = TallAccordion (rm + delta) rs
      mainResize Shrink = TallAccordion (rm - delta) rs
      sideResize VExpand = TallAccordion rm (rs + delta)
      sideResize VShrink = TallAccordion rm (rs - delta)

data VExpand = VExpand | VShrink

instance Message VExpand

data ToggleReflect w = TRActive | TRInactive
  deriving (Eq, Show, Read)

data ToggleReflectMsg = ToggleReflectMsg
instance Message ToggleReflectMsg

instance LM.LayoutModifier ToggleReflect Window where 
  pureModifier _ _ Nothing layout = (layout, Nothing)
  pureModifier TRInactive _ _ layout = (layout, Nothing)
  pureModifier TRActive (Rectangle sx sy sw sh) _ layout = (fmap flip <$> layout, Nothing)
    where
      flip (Rectangle x y w h) = Rectangle (fromIntegral sw - x - fromIntegral w) y w h
  pureMess tr msg = toggle tr <$> fromMessage msg
    where
      toggle TRActive ToggleReflectMsg = TRInactive
      toggle TRInactive ToggleReflectMsg = TRActive

toggleReflect = LM.ModifiedLayout TRInactive

data ToggleZoom w = TZActive | TZInactive
  deriving (Eq, Read, Show)

data ToggleZoomMsg = ToggleZoom

instance LM.LayoutModifier ToggleZoom Window where
  pureModifier _ _ Nothing layout = (layout, Nothing)
  pureModifier TZInactive _ _ layout = (layout, Nothing)
  pureModifier TZActive screen (Just stk) layout = 
    let (fg, bg) = partition ((== W.focus stk) . fst) layout
        fg' = fmap (\(wid, rect) -> (wid, interp 0.5 rect screen)) fg
     in (fg' ++ bg, Nothing)

  pureMess tc msg = tgl tc <$> fromMessage msg
    where
      tgl TZActive ToggleZoom = TZInactive
      tgl TZInactive ToggleZoom = TZActive

instance Message ToggleZoomMsg

interp :: Double -> Rectangle -> Rectangle -> Rectangle
interp r (Rectangle xa ya wa ha) (Rectangle xb yb wb hb) = Rectangle (s xa xb) (s ya yb) (s wa wb) (s ha hb)
  where
    s :: Integral a => a -> a -> a
    s a b = round $ fromIntegral a * (1-r) + fromIntegral b * r

toggleZoom = LM.ModifiedLayout TZInactive

defaultSpacing, spacingDelta :: Int -- global constant to share value between reset key and layout
defaultSpacing = 60
spacingDelta = 15

-- myLayout = avoidStruts $ toggleZoom $ SP.spacingWithEdge defaultSpacing $ Tall 1 (3/100) (3/5)
myLayout = avoidStruts $ toggleReflect $ toggleZoom $ SP.spacingWithEdge defaultSpacing $ TallAccordion (3/5) (3/5)

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
      , ((m, xK_q), spawn "xmonad --restart") -- overrides the default behavior, which first recompiles
      , ((m, xK_d), withPwd $ maybe (spawn "dolphin") (\pwd -> spawn $ "dolphin " <> pwd))
      , ((m .|. shiftMask, xK_f), spawn "clipboard-firefox")
      , ((m, xK_Return), mkTerm "/etc/profiles/per-user/jmc/bin/fish")
      , ((m .|. ctrlMask, xK_Return), mkTerm "/etc/profiles/per-user/jmc/bin/fish")
      , ((m, xK_z), sendMessage ToggleZoom)
      , ((m, xK_m), sendMessage ToggleReflectMsg)
      , ((m .|. shiftMask, xK_h), sendMessage VShrink)
      , ((m .|. shiftMask, xK_l), sendMessage VExpand)
      , ((m .|. shiftMask, xK_Return), windows W.swapMaster)
      , ((m, xK_s), windows W.swapDown >> windows W.focusUp)
      , ((m, xK_c), spawn "emacsclient --create-frame --no-wait")
      , ((m, xK_p), spawn "passmenu")
      , ((m.|. shiftMask, xK_p), spawn "qtpass")
      -- , ((m, xK_c), spawn "emacs")
      , ((m, xK_grave), myCycleRecentWS [xK_Super_L] xK_grave xK_grave)
      -- , ((m, xK_grave), myCycleRecentWS [xK_Super_R] xK_grave xK_grave)
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
