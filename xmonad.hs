import System.Exit
import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts,manageDocks,docksEventHook)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops 

import XMonad.Actions.CycleWS
import XMonad.Actions.TopicSpace
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

import XMonad.Layout.AutoMaster
import XMonad.Layout.Circle
--import XMonad.Layout.PerWorkspace
import XMonad.Layout.MagicFocus
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import XMonad.Prompt.Workspace
import XMonad.Prompt


main :: IO ()
main = xmonad =<< myStatusBar myConfig

myConfig = defaultConfig { terminal = "urxvt"
                         , modMask = mod4Mask
                         , focusFollowsMouse = False
                         , workspaces = myWorkspaces
                         , borderWidth = 2
                         , manageHook = myManageHook
                         , handleEventHook = myEventHook
                         , layoutHook = myLayout
                         , keys = \c -> mkKeymap c (myKeyMap c)
                         }

---------------------------------------------
-- 新規ウインド配置の管理 manageHook
---------------------------------------------
myManageHook =  scPadManageHook <+>
  composeOne ( [ isFullscreen -?> doFullFloat
                , transience
               ]
            ++ [ title   =? "オプション" -?> doCenterFloat ]
            ++ [ className =? c -?> doCenterFloat | c <- center_floats]
            ++ [ className =? c -?> doFloat       | c <- floats])
    where
      center_floats =
        [ "MPlayer"
        , "feh"
        , "Xmessage"
        ]

      floats = 
        ["Gimp"]

      scPadManageHook = namedScratchpadManageHook scratchpads

---------------------------------------------
-- イベントフック
---------------------------------------------
myEventHook = handleEventHook defaultConfig <+> fullscreenEventHook

---------------------------------------------
-- キーバインド関連
---------------------------------------------
myKeyMap :: XConfig Layout -> [(String, X ())]
myKeyMap conf =
  [("M-<Return>", spawnShell)
  ,("M-p", spawn "dmenu_run")
  ,("M-S-c", kill)
         
  ,("M-<Space>", sendMessage NextLayout)

  ,("M-S-<Space>", setLayout $  XMonad.layoutHook conf)
  
  ,("M-n", refresh)
  
  ,("M-<KP_Tab>", windows W.focusDown)
  ,("M-S-<KP_Tab>", windows W.focusUp)
  ,("M-j", windows W.focusDown)
  ,("M-k", windows W.focusUp)
  ,("M-m", windows W.focusMaster)

  ,("M-S-j", windows W.swapDown)
  ,("M-S-k", windows W.swapUp)
  ,("M-S-<Return>", windows W.swapMaster)
  
  ,("M-h", sendMessage Shrink)
  ,("M-l", sendMessage Expand)

  ,("M-t", withFocused $ windows . W.sink)

  ,("M-,", sendMessage $ IncMasterN 1)
  ,("M-.", sendMessage $ IncMasterN (-1))
  ,("M-S-q", io exitSuccess)
  ,("M-q", spawn myRecompileCmd )
  
  ,("M-<R>", DO.moveTo Next HiddenNonEmptyWS)
  ,("M-<L>", DO.moveTo Prev HiddenNonEmptyWS)
  ,("M-C-<R>", DO.moveTo Next AnyWS)
  ,("M-C-<L>", DO.moveTo Prev AnyWS)
  
  ,("M-o", namedScratchpadAction scratchpads "ScratchPad")
  ,("M-i", namedScratchpadAction scratchpads "ScratchPad2")
  ]

  -- workspaceの移動等
  ++
  [("M-" ++ m ++ show k , f i)
    | (i,k) <- zip (XMonad.workspaces conf) [1..9]
    , (f,m) <- [(goto, ""),(windows . W.shift, "S-")]
  ]

  ++
  [("M-g",   promptedGoto)
  ,("M-S-g", promptedShift)
  ]

    where
      myRecompileCmd =
        "xmonad --recompile && (killall dzen2; xmonad --restart) && xmessage recompile done"


---------------------------------------------
-- ターミナル関連
---------------------------------------------
customTerm1 = "urxvt -depth 32 -bg '[80]#003f3f' "
customTerm2 = "urxvt -depth 32 -bg '[80]#3f003f' "

---------------------------------------------
-- スクラッチパッド 
---------------------------------------------
scratchpads =
  [ NS "ScratchPad" (customTerm1 ++ "-title ScratchPad") (title =? "ScratchPad") centerfloat
  , NS "ScratchPad2" (customTerm2 ++ "-title ScratchPad2") (title =? "ScratchPad2") defaultFloating
  ]
  where centerfloat = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

---------------------------------------------
-- レイアウト関連
---------------------------------------------
myLayout = smartBorders . renamed [CutWordsLeft 2] . spacing 4 $ myBaseLayout

myBaseLayout = renamed [Replace "MT"] (Mirror $ Tall 2 (3/100) (3/5))
          ||| renamed [Replace "MaT"] (Mirror $ magicFocus $ Tall 1 (3/100) (4/5))
          ||| renamed [Replace "T" ] (Tall 1 (3/100) (3/5))
          ||| renamed [Replace "F" ] Full
          ||| magicFocus (autoMaster 1 (3/100) Circle)


---------------------------------------------
-- ワークスペースとトピックスペース
---------------------------------------------
data TopicItem = TI { topicName   :: Topic
                    , topicDir    :: Dir 
                    , topicAction :: X ()
                    }

mkTopicConfig :: [TopicItem] -> TopicConfig
mkTopicConfig items@(x:xs)  = 
  TopicConfig { topicDirs    = M.fromList [(t,d)| TI t d _ <- items]
              , topicActions = M.fromList [(t,x)| TI t _ x <- items]
              , defaultTopicAction = const $ return ()
              , defaultTopic = topicName x
              , maxTopicHistory = 10
              }

-- mytopicにデータを設定する
mytopics = 
  [ TI "home"       ""            spawnShell
  , TI "misc"       ""            spawnShell
  , TI "code"       "~/code"      (spawnShell >*> 2)
  , TI "tmp"        "~/tmp"       spawnShell
  , TI "web"        ""            (spawn "chromium")
  , TI "firefox"    ""            (spawn "firefox")
  , TI "navi2ch"    ""            (spawn "emacs -f navi2ch")
  , TI "skype"      ""            (spawn "skype")
  , TI "network"    ""            (spawn $ locale_en ++ "urxvt -e wicd-curses")
  , TI "xmonad"     "~/.xmonad"   spawnShell
   ]

myWorkspaces = [ n | TI n _ _ <- mytopics]

myTopicData = mkTopicConfig mytopics

-- *************************
-- topicspace用のアクション
-- *************************

-- ターミナルを開くアクション
spawnShell :: X ()
spawnShell  = currentTopicDir myTopicData >>= spawnShellIn

-- 任意のディレクトリに移動してターミナルを開くアクション
spawnShellIn :: Dir -> X()
spawnShellIn [] = spawn $ locale_en ++ "urxvt -cd ~/" 
spawnShellIn d  = spawn $ locale_en ++ "urxvt -cd " ++ d

locale_en = "LANG=en_US.UTF-8 "

-- *************************
-- ワークスペース移動用（キーバインドする）
-- *************************

-- 任意のトピックスペースに移動
goto :: Topic -> X ()
goto = switchTopic myTopicData

-- ワークスペースプロンプトを使って移動
promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

-- ワークスペースプロンプトでウインドを移動
promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig (windows . W.shift)

myXPConfig = defaultXPConfig
    { fgColor = "orange"
    , bgColor = "black"
    , font    = "xft:Ricty:pixelsize=20:autohint=true"
    , position = Bottom
    , height   = 24
    }


---------------------------------------------
-- ステータスバー関連
---------------------------------------------
myStatusBar conf = do
  left_bar <- spawnPipe $ "dzen2 -x 0 -w 700 -ta l " ++ common_style
  spawn $ "conky -c ~/.xmonad/conky_dzen | dzen2 -x 700 -ta r " ++ common_style
  return $ conf { layoutHook = avoidStruts $ layoutHook conf
                , manageHook = manageHook conf <+> manageDocks
                , handleEventHook = handleEventHook conf <+> docksEventHook
                , logHook    = dynamicLogWithPP $  myDzenPP left_bar
                }
    where
      -- dzenのオプションの共通部分
      common_style = "-h '20' -fg '#aaaaaa' -bg '#000000' -fn 'M+ 1mn:size=10'"
      -- ppカスタマイズ
      myDzenPP h = defaultPP { ppCurrent = dzenColor "#00ffaa" "" . wrap "[" "]"
                             , ppHidden  = dzenColor "#00aa11" "" . wrap "" ""
                             , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
                             , ppSep     = " : "
                             , ppLayout  = dzenColor "#aaaaaa" ""
                             , ppSort    = fmap ( . namedScratchpadFilterOutWorkspace) (ppSort defaultPP)
                             , ppOutput  = hPutStrLn h
                             }

