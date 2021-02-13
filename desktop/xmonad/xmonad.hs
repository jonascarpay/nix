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
import Data.List (findIndex, isPrefixOf)
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
import qualified XMonad.Layout.Spacing as SP
import qualified XMonad.StackSet as W
import qualified XMonad.Util.WorkspaceCompare as WC
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.Layout.LayoutModifier as LM
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import qualified XMonad.Util.Rectangle as UR

altMask = mod1Mask

ctrlMask = controlMask

-- termEmu = "termite -e /home/jmc/.nix-profile/bin/fish"
termEmu = "st"
-- termEmu = "urxvt -e /home/jmc/.nix-profile/bin/fish"

data TallDock a = TallDock
  { tdNDock :: !Int
  , tdRatioDock :: !Rational
  , tdRatioSide :: !Rational
  , tdZoomTall :: !Bool
  }
  deriving (Show, Read)

-- TODO cycle
instance LayoutClass TallDock a where
  description _ = "TallDock"

  pureLayout (TallDock ndock rd rs zoom) rect (W.Stack x pre post) =
      zip (fst <$> wins) rects
    where
      wins :: [(a, Bool)]
      wins = ((,False) <$> reverse pre) <> [(x, True)] <> ((,False) <$> post)
      ([main], rest) = splitAt 1 wins
      (tall, dock) = splitAt (length rest - ndock) rest

      splitTall :: Rectangle -> [Rectangle]
      splitTall rect
        | zoom, Just n <- findIndex snd tall
          = let rects = splitVertically (ntall + weight - 1) rect
                (pre,(focus, post)) = splitAt weight <$> splitAt n rects
             in pre <> (stack focus : post)
        | otherwise = splitVertically ntall rect
        where
          stack :: [Rectangle] -> Rectangle
          stack rects@(Rectangle x y w _ :_) = Rectangle x y w (sum $ rect_height <$> rects)
          ntall = length tall
          weight = 3

      rects =
        let (left, tallrects)
              | null tall = (rect, []) 
              | otherwise = splitTall <$> splitHorizontallyBy rs rect
            (mainrect, dockrects)
              | null dock = (left, []) 
              | otherwise = (reverse . splitHorizontally (length dock)) <$> splitVerticallyBy rd left
         in mainrect : tallrects <> dockrects

  pureMessage (TallDock m rd rs z) msg
      =   fmap resize (fromMessage msg)
      <|> fmap vresize (fromMessage msg)
      <|> fmap incDock (fromMessage msg)
      <|> fmap zoomToggle (fromMessage msg)

    where
      delta = 3 / 100
      resize Shrink = TallDock m rd (max 0.1 $ rs-delta) z
      resize Expand = TallDock m rd (min 0.9 $ rs+delta) z
      vresize VShrink = TallDock m (max 0.1 $ rd-delta) rs z
      vresize VExpand = TallDock m (min 0.9 $ rd+delta) rs z
      incDock (IncMasterN d) = TallDock (max 0 (m+d)) rd rs z
      zoomToggle ZoomToggle = TallDock m rd rs (not z)

data ZoomToggle = ZoomToggle

instance Message ZoomToggle

defaultTallDock :: TallDock a
defaultTallDock = TallDock 0 (3/4) (3/5) False

data LiftFocused a = LiftFocused Int32 Int32
  deriving (Eq, Read, Show)

instance LM.LayoutModifier LiftFocused Window where
  redoLayout (LiftFocused dx dy) rect _ wins = do
      curr <- gets (W.stack . W.workspace . W.current . windowset)
      case curr of
        Just (W.Stack wid _ _) ->
          pure $ (fmap (zoom wid) wins, Nothing)
        Nothing -> pure (wins, Nothing)
    where
      offsetRect dx dy (Rectangle x y w h) = Rectangle (x+dx) (y+dy) w h
      zoom needle (wid, rect)
        | needle == wid = (wid, offsetRect dx dy rect)
        | otherwise = (wid, rect)

liftFocused :: Int32 -> Int32 -> l a -> LM.ModifiedLayout LiftFocused l a
liftFocused dx dy = LM.ModifiedLayout (LiftFocused dx dy)

defaultSpacing, spacingDelta :: Int -- global constant to share value between reset key and layout
defaultSpacing = 60
spacingDelta = 15

-- myLayout = liftFocused (-15) (-17) $ avoidStruts $ spacingWithEdge 30  $ defaultTallDock
myLayout = avoidStruts $ SP.spacingWithEdge defaultSpacing $ defaultTallDock

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
  -- forM [".xmonad-worspace-log", ".xmonad-title-log"] $ \file ->
  --   safeSpawn "mkfifo" ["/tmp/" ++ file]
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

myKeys conf = M.fromList myKeyList <> keys desktopConfig conf
  where
    m = modMask conf
    myKeyList = 
      [ ((m, xK_f), spawn "firefox")
      , ((m, xK_d), withPwd $ maybe (spawn "dolphin") (\pwd -> spawn $ "dolphin " <> pwd))
      , ((m .|. shiftMask, xK_f), spawn "clipboard-firefox")
      , ((m, xK_Return), mkTerm "/home/jmc/.nix-profile/bin/fish")
      , ((m .|. ctrlMask, xK_Return), mkTerm "/home/jmc/.nix-profile/bin/tmux")
      , ((m, xK_z), sendMessage ZoomToggle)
      , ((m .|. shiftMask, xK_Return), windows W.swapMaster)
      , ((m, xK_s), windows W.swapDown >> windows W.focusUp)
      , ((m, xK_c), spawn "emacsclient --create-frame --no-wait")
      -- , ((m, xK_c), spawn "emacs")
      , ((m .|. shiftMask, xK_h), sendMessage VShrink)
      , ((m .|. shiftMask, xK_l), sendMessage VExpand)
      , ((m, xK_n), CW.moveTo CW.Next CW.HiddenWS)
      , ((m, xK_p), CW.moveTo CW.Prev CW.HiddenWS)
      , ((m .|. controlMask, xK_n), CW.shiftTo CW.Next CW.HiddenWS)
      , ((m .|. controlMask, xK_p), CW.shiftTo CW.Prev CW.HiddenWS)
      , ((m .|. shiftMask, xK_n), CW.shiftTo CW.Next CW.HiddenWS >> CW.moveTo CW.Next CW.HiddenNonEmptyWS)
      , ((m .|. shiftMask, xK_p), CW.shiftTo CW.Prev CW.HiddenWS >> CW.moveTo CW.Prev CW.HiddenNonEmptyWS)
      , ((m, xK_grave), CW.moveTo CW.Next CW.HiddenNonEmptyWS)
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
