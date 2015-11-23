import XMonad
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.PerWorkspace
import qualified XMonad.Actions.TopicSpace as TS
import qualified XMonad.StackSet as W
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import qualified Data.Map as M
import Control.Monad
import XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Actions.SpawnOn
import XMonad.Hooks.SetWMName


keyBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 [ ((modm, xK_grave) , goto "main")
 , ((modm .|. shiftMask, xK_grave) , move "main")
 , ((modm, xK_F1) , goto "mail")
 , ((modm, xK_F2) , goto "chat")
 , ((0, 0x1008ff13) , spawn "amixer set Master 10%+")
 , ((0, 0x1008ff11) , spawn "amixer set Master 10%-") 
 , ((0, 0x1008ff12) , spawn "amixer set Master toggle")
 , ((modm, xK_Menu) , spawn "bmpc toggle")
 , ((modm, xK_Alt_R) , spawn "bmpc prev")
 , ((modm, xK_Control_R) , spawn "bmpc next")
 ]


main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { modMask 	= mod4Mask 
    , terminal	= "urxvt"
    , keys = \c -> keyBindings c `M.union` keys defaultConfig c
    , layoutHook =  avoidStruts
                    $ onWorkspace "adom" (Mirror $ Tall 1 (3/100) (80/100))
                    $ onWorkspace "chat" (Tall 1 (3/100) (1/2))
                    $ onWorkspace "mail" Full
		    $ onWorkspace "main" Grid
                    $ onWorkspace "ff" Full
                    $ onWorkspace "cr" Full
                    $ onWorkspace "org" Full
                    $ onWorkspace "conf" (avoidStruts Full) 
                    $ onWorkspace "_" Grid
                    $ avoidStruts (tiled ||| Mirror tiled ||| Full)
    -- , manageHook	= manageHook
    , logHook =dynamicLogWithPP xmobarPP
      {
      ppOutput = hPutStrLn xmproc
      }
    , startupHook = do 
        setWMName "LG3D"
	spawn "amixer set Master 0%"
	spawn "mpd"
    }
    where tiled = Tall 1 (3/100) (1/2)

_spaces = M.fromList $
 [ ("org" , "~/org")
 , ("pva" , "~/pva")
 ]

_topicConfig = TS.TopicConfig
 { TS.topicDirs = _spaces
 , TS.topicActions = _topicActions
 , TS.defaultTopicAction = (const $ return ())
 , TS.defaultTopic = "main"
 , TS.maxTopicHistory = 10
 }

_topicActions = M.fromList $
 [ ("main", runConsole "htop"
         >> runConsole "ncmpcpp"
	 >> runConsole "ifconfig"
	 >> runVim ""
	 >> runVim ""
	 >> runVim "")
 , ("org", runEMACS "-e \"(gtd)\"")
 , ("conf", runVim' ["~/.xmonad/xmonad.hs", "~/.Xdefaults"] )
 , ("mc", runConsole "ranger" )
 , ("cr", spawn "google-chrome-stable" )
 , ("mail", spawnHere "thunderbird" )
 , ("chat", spawnHere "chromium web.whatsapp.com"
         >> spawnHere "telegram" )
 , ("telegram", runConsole "telegram" )
 , ("pva", runPDF "~/pva/pva.pdf"
        >> runTerminal' "~/pva"
        >> runVim "~/pva/pva.tex")
 ]

goto :: WorkspaceId -> X ()
goto t = newWorkspace t >> TS.switchTopic _topicConfig t

move :: WorkspaceId -> X ()
move t = newWorkspace t >> (windows $ W.shift t)

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do exists <- widExist w
                    if (not exists) then DW.addHiddenWorkspace w else return ()

widExist :: WorkspaceId -> X Bool
widExist wid = do xs <- get
                  return $ widExists wid (windowset xs)
 
widExists :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
widExists wid ws = wid `elem` map W.tag (W.workspaces ws)


runTerminal :: X ()
runTerminal', runConsole, runVim, runPDF, runEMACS :: String -> X ()
runVim' :: [String] -> X ()
runTerminal = spawn "urxvt"
runConsole prog = spawn ("urxvt -e " ++ prog)
runVim file = spawn ("urxvt -e vim " ++ file)
runVim' = undefined
runPDF = undefined
runEMACS = undefined
runTerminal' = undefined
