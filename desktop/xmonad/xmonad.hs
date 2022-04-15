{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -Wno-deprecations -Wno-name-shadowing -Wno-missing-signatures #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (isPrefixOf, isSuffixOf, partition, sortOn)
import qualified Data.Map as M
import Data.Maybe (isJust)
import XMonad
import qualified XMonad.Actions.CycleRecentWS as CW
import qualified XMonad.Actions.Submap as SM
import XMonad.Config.Desktop as DC
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Hooks.EwmhDesktops as EMWH
import qualified XMonad.Hooks.ManageDocks as MD
import qualified XMonad.Hooks.ManageHelpers as MH
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.Layout.Spacing as SP
import qualified XMonad.StackSet as W
import XMonad.Util.Run

data TallAccordion a = TallAccordion !Rational !Rational
  deriving (Eq, Show, Read)

instance LayoutClass TallAccordion a where
  pureLayout (TallAccordion rm rs) screen stk@(W.Stack cur pre post) =
    case stk of
      (W.Stack _ [] []) -> [(cur, screen)]
      (W.Stack _ [wmain] []) -> [(cur, side), (wmain, prim)]
      (W.Stack _ [] [wside]) -> [(cur, prim), (wside, side)]
      _ -> (cur, rcur) : zip (reverse pre) rpre <> zip post rpost
    where
      n = length pre + length post + 1

      (prim, side) = splitHorizontallyBy rm screen
      (sec, rest) = splitVertically (n -2) <$> splitVerticallyBy rs side

      rects = prim : sec : rest
      (rpre, rcur : rpost) = splitAt (length pre) rects

  pureMessage (TallAccordion rm rs) msg = fmap mainResize (fromMessage msg) <|> fmap sideResize (fromMessage msg)
    where
      delta = 1 / 10
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
  pureModifier TRActive (Rectangle _ _ sw _) _ layout = (fmap flip <$> layout, Nothing)
    where
      flip (Rectangle x y w h) = Rectangle (fromIntegral sw - x - fromIntegral w) y w h
  pureMess tr msg = toggle tr <$> fromMessage msg
    where
      toggle TRActive ToggleReflectMsg = TRInactive
      toggle TRInactive ToggleReflectMsg = TRActive

toggleReflect :: l a -> LM.ModifiedLayout ToggleReflect l a
toggleReflect = LM.ModifiedLayout TRInactive

data ToggleZoom w = TZActive | TZInactive
  deriving (Eq, Read, Show)

data ToggleZoomMsg = ToggleZoom

data ResetWhenEmpty m a = ResetWhenEmpty
  { rweDefault :: m a,
    rweCurrent :: m a
  }
  deriving (Read, Show)

instance LayoutClass m a => LayoutClass (ResetWhenEmpty m) a where
  description = description . rweCurrent
  handleMessage (ResetWhenEmpty def cur) msg = (fmap . fmap) (ResetWhenEmpty def) (handleMessage cur msg)
  runLayout (W.Workspace tag (ResetWhenEmpty def cur) mstack) rect =
    case mstack of
      Nothing -> pure ([], Just (ResetWhenEmpty def def))
      Just _ -> (fmap . fmap . fmap) (ResetWhenEmpty def) (runLayout (W.Workspace tag cur mstack) rect)

resetWhenEmpty :: m a -> ResetWhenEmpty m a
resetWhenEmpty l = ResetWhenEmpty l l

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
    s a b = round $ fromIntegral a * (1 - r) + fromIntegral b * r

toggleZoom :: l a -> LM.ModifiedLayout ToggleZoom l a
toggleZoom = LM.ModifiedLayout TZInactive

defaultSpacing, spacingDelta :: Int -- global constant to share value between reset key and layout
defaultSpacing = 45
spacingDelta = 15

myLayout = MD.avoidStruts $ toggleReflect $ toggleZoom $ SP.spacingWithEdge defaultSpacing $ TallAccordion (3 / 5) (3 / 5)

main :: IO ()
main = do
  dbus <- D.connectSession
  void $
    D.requestName
      dbus
      (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad $
    EMWH.ewmh $
      dcfg dbus

cycleNonFocusDown :: W.Stack a -> W.Stack a
cycleNonFocusDown (W.Stack f us ds) = W.Stack f us' (reverse ds')
  where
    wins = us <> reverse ds
    wins' =
      let (h, t) = splitAt 1 wins
       in t <> h
    (us', ds') = splitAt (length us) wins'

cycleNonFocusUp :: W.Stack a -> W.Stack a
cycleNonFocusUp (W.Stack f us ds) = W.Stack f (reverse us') ds'
  where
    wins = ds <> reverse us
    wins' =
      let (h, t) = splitAt 1 wins
       in t <> h
    (ds', us') = splitAt (length ds) wins'

dcfg dbus =
  desktopConfig
    { terminal = "st",
      modMask = mod4Mask,
      borderWidth = 0, -- Necessary to remove borders from floating windows
      focusFollowsMouse = False,
      logHook = polybar dbus,
      layoutHook = resetWhenEmpty $ myLayout ||| Full,
      manageHook =
        composeAll
          [ manageHook desktopConfig,
            className =? "Gnome-calculator" --> MH.doCenterFloat,
            className =? "Pavucontrol" --> MH.doCenterFloat
          ],
      keys = myKeys,
      workspaces = show <$> take 9 [1 :: Int ..],
      handleEventHook = handleEventHook def <+> EMWH.fullscreenEventHook
    }

myCycleRecentWS :: [KeySym] -> KeySym -> KeySym -> X ()
myCycleRecentWS = CW.cycleWindowSets options
  where
    options w = map (flip W.view w) (recentTags w)
    recentTags = fmap W.tag . filterNonScratch . filterNonEmpty . rotate . W.workspaces
      where
        filterNonScratch ws =
          case filter ((<= (5 :: Int)) . read . W.tag) ws of
            ws'@(_ : _ : _) -> ws'
            _ -> ws
        filterNonEmpty ws = case filter (isJust . W.stack) ws of
          [] -> ws
          ws' -> ws'
        rotate (x : xs) = xs ++ [x]
        rotate _ = error "no worspaces?"

myKeys conf = M.fromList myKeyList <> keys desktopConfig conf
  where
    m = modMask conf
    myKeyList =
      [ ((m, xK_f), spawn "firefox"),
        ((m, xK_q), spawn "xmonad --restart"), -- overrides the default behavior, which first recompiles
        ((m, xK_d), withPwd $ maybe (spawn "dolphin") (\pwd -> spawn $ "dolphin " <> pwd)),
        ((m .|. shiftMask, xK_f), spawn "clipboard-firefox"),
        ((m, xK_Return), mkTerm "/etc/profiles/per-user/jmc/bin/fish"),
        ((m, xK_z), sendMessage ToggleZoom),
        ((m, xK_m), sendMessage ToggleReflectMsg),
        ((m .|. shiftMask, xK_h), sendMessage VShrink),
        ((m .|. shiftMask, xK_l), sendMessage VExpand),
        ((m .|. shiftMask, xK_Return), windows W.swapMaster),
        ((m, xK_s), windows W.swapDown >> windows W.focusUp),
        ((m, xK_c), spawn "emacsclient --create-frame --no-wait"),
        ((m, xK_s), spawn "dmenu-web-search"),
        ((m, xK_o), spawn "dmenu-run"),
        ( (m, xK_p),
          SM.submap . M.fromList $
            [ ((m, xK_p), spawn "dmenu-pass"),
              ((m, xK_d), spawn "dmenu-directory"),
              ((m, xK_r), spawn "dmenu-command")
            ]
        ),
        ((m .|. shiftMask, xK_p), spawn "qtpass"),
        -- , ((m, xK_c), spawn "emacs")
        ((m, xK_grave), myCycleRecentWS [xK_Super_L] xK_grave xK_grave), -- TODO switch to cycleRecentNonEmptyWS from xmonad-contrib 0.17
        ((m, xK_Escape), myCycleRecentWS [xK_Super_L] xK_Escape xK_Escape), -- TODO switch to cycleRecentNonEmptyWS from xmonad-contrib 0.17
        ((m, xK_y), windows $ W.modify' cycleNonFocusDown),
        ((m .|. shiftMask, xK_y), windows $ W.modify' cycleNonFocusUp),
        -- Border control
        ((m, xK_minus), SP.incScreenWindowSpacing $ fromIntegral spacingDelta),
        ((m, xK_equal), SP.decScreenWindowSpacing $ fromIntegral spacingDelta),
        ((m, xK_0), SP.setScreenWindowSpacing $ fromIntegral defaultSpacing)
      ]
        <> [ ((m .|. mask, k), windows f)
             | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9],
               (mask, f) <-
                 [ (0, W.greedyView i),
                   (shiftMask, W.shift i),
                   (controlMask, W.greedyView i . W.shift i)
                 ]
           ]
        <> [ ((m .|. mask, key), screenWorkspace scr >>= flip whenJust (windows . f))
             | (key, scr) <- zip [xK_w, xK_e, xK_r] [0 ..],
               -- . | (key, scr) <- zip [xK_e, xK_w] [0..]
               (mask, f) <-
                 [ (0, W.view),
                   (shiftMask, W.shift),
                   (controlMask, \n -> W.view n . W.shift n) -- TODO greedyview vs view?
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

polybar :: D.Client -> X ()
polybar dbus = do
  winset <- gets windowset
  workspaceIcons <- do
    namedWindows <- (traverse . traverse . traverse) getHints (windows' winset)
    let currentWS :: Int = read . W.currentTag $ winset
    pure $ formatWorkspaces currentWS namedWindows
  title <- case fmap W.focus . W.stack . W.workspace . W.current $ winset of
    Nothing -> pure id
    Just wid -> do
      str <- runQuery title wid
      pure (showString " - " . bold (showString (DL.shorten 80 str)))
  io $ dbusOutput dbus (showString " " . workspaceIcons . title $ "")
  where
    bold :: ShowS -> ShowS
    bold = wrap "%{T2}" "%{T-}"

    formatWorkspaces :: Int -> [(Int, [(String, String)])] -> ShowS
    formatWorkspaces cur wss = pre . post
      where
        cat :: [ShowS] -> ShowS
        cat = foldr (.) id

        (wsAlways, wsExtra) = splitAt 5 (sortOn fst wss)

        pre = cat (uncurry formatWorkspace <$> wsAlways)
        post =
          let nonEmptyExtra = filter (\(wid, wins) -> wid == cur || not (null wins)) wsExtra
           in if null nonEmptyExtra && cur <= 5
                then id
                else showString " | " . cat (uncurry formatWorkspace <$> nonEmptyExtra)

        formatWorkspace :: Int -> [(String, String)] -> ShowS
        formatWorkspace n wins = style $ showChar ' ' . shows n . (if null wins then id else showChar ' ') . wins' . showChar ' '
          where
            wins' = formatWindows wins
            style
              | cur == n = wrap "%{R}" "%{R-}"
              | null wins = wrap "%{F#81a1c1}" "%{F-}"
              | otherwise = id

        formatWindows :: [(String, String)] -> ShowS
        formatWindows = cat . fmap (\win -> showChar (uncurry iconize win) . showChar ' ')

    wrap :: String -> String -> ShowS -> ShowS
    wrap p q i = showString p . i . showString q

    windows' :: W.StackSet WorkspaceId a Window b c -> [(Int, [Window])]
    windows' = fmap (\(W.Workspace i _ stk) -> (read i, maybe [] W.integrate stk)) . W.workspaces

    getHints :: Window -> X (String, String)
    getHints = runQuery (liftA2 (,) title className)

iconize :: String -> String -> Char
iconize title "firefox"
  | "YouTube — Mozilla Firefox" `isSuffixOf` title = '\xFAC2'
  | "Wikipedia — Mozilla Firefox" `isSuffixOf` title = '\xF266'
  | "Hacker News — Mozilla Firefox" `isSuffixOf` title = '\xF1D4'
  | "GitHub — Mozilla Firefox" `isSuffixOf` title = '\xF408'
  | "Twitter — Mozilla Firefox" `isSuffixOf` title = '\xF099'
  | "WhatsApp — Mozilla Firefox" `isSuffixOf` title = '\xFAA2'
  | otherwise = '\xF269'
iconize title "st-256color"
  | "vim" `isPrefixOf` title = '\xE62B'
  | "htop" `isPrefixOf` title = '\xF080'
  | "hoogle" `isPrefixOf` title = '\xF98A'
  | "pydoc" `isPrefixOf` title = '\xF98A'
  | "ranger" `isPrefixOf` title = '\xF0DB' -- 
  | "tmux" `isPrefixOf` title = '\xFB3F' -- ﬿
  | otherwise = '\xE795' -- 
iconize _ "Slack" = '\xF198'
iconize _ "Spotify" = '\xF1BC'
iconize _ "Google-chrome" = '\xF268'
iconize _ "Chromium-browser" = '\xF7AE'
iconize _ "Blender" = '\xF5AA'
iconize _ "Steam" = '\xF1B7'
iconize _ "TelegramDesktop" = '\xF2C6'
iconize _ "Signal" = '\xF860'
iconize _ _ = '\xF2D0'

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
    signal = (D.signal objectPath interfaceName memberName) {D.signalBody = [D.toVariant str]}
