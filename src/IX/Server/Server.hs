module IX.Server.Server 
   (server
   ,port) where

import Network.Socket (socket
                      ,Family ( AF_INET )
                      ,SocketType ( Stream )
                      ,defaultProtocol
                      ,SockAddr ( SockAddrInet )
                      ,bindSocket
                      ,listen
                      ,accept
                      ,socketToHandle)
import Safe (lookupJustNote,readMay)
import DataStructures.Composite
import DataStructures.Atomic
import IX.Reactive.EventNetwork (gameloop)
import IX.Universe.Input (initAmap
                         ,initPmap
                         ,initLmaps)
import IX.Universe.HyperSpace (getName)
import IX.Universe.Utils      (getMessage)
import Network (withSocketsDo,sClose,PortNumber,Socket)
import Control.Concurrent.STM.TChan (newTChan
                                    ,TChan
                                    ,newTChan
                                    ,writeTChan
                                    ,readTChan)
import Control.Concurrent.STM.TMVar (newTMVar
                                    ,TMVar
                                    ,readTMVar
                                    ,swapTMVar
                                    ,takeTMVar
                                    ,putTMVar)
import Control.Monad.STM (STM,atomically)
--import System.Environment (getArgs)
import System.IO (IOMode (ReadWriteMode)
                 ,IO 
                 ,Handle
                 ,hSetBuffering
                 ,BufferMode (LineBuffering)
                 ,hIsEOF
                 ,hClose
                 ,hGetLine
                 ,hPutStrLn
                 ,putStrLn)

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (Monad,forever,(>>),return,mapM_)
import Data.List (map,zip,length)
import Data.List.Utils (addToAL)
import Data.String (String,words,unwords)
import Data.Bool (Bool (True))
import Data.Text (Text,pack,unpack)
import Data.Maybe (Maybe (..),mapMaybe)
import Data.Int (Int)
import Data.Tuple (swap)
--import Data.Char (toLower,toUpper)
import Text.Show (show)
import Prelude (fromIntegral,($),(++),(.),(-),(+),fst)

server :: Int -> IO ()
server port' = withSocketsDo $ do
   bracket (prepareSocket $ fromIntegral port' )
           sClose
           acceptConnections
   return ()

prepareSocket :: PortNumber -> IO Socket
prepareSocket port' = do
   sock' <- socket AF_INET Stream defaultProtocol
   let socketAddress = SockAddrInet port' 0000
   bindSocket sock' socketAddress
   listen sock' 1
   putStrLn $ "Listening on " ++ (show port)
   return sock'

acceptConnections :: Socket -> IO ()
acceptConnections sock' = do
   gameData <- atomically $ mkGameData
   forever $ do
      (sock, _) <- Network.Socket.accept sock'
      handle <- socketToHandle sock ReadWriteMode
      sockHandler handle gameData
   where
     mkGameData :: STM GameData
     mkGameData = do
        -- g2p: how the game knows how to communicate with players
        g2p  <- newTMVar [] :: STM (TMVar [(Name,Handle)])
        -- Player to Player Interface
        p2p  <- newTMVar [] :: STM (TMVar [(Name,AID)])
        -- permToGo: manages command flooding
        permToGo <- newTMVar [] :: STM (TMVar [(AID,Bool)])
        -- cChan: Player input 
        cChan    <- newTChan :: STM (TChan [UAC])
        -- gState: Game  output
        gState   <- newTChan :: STM (TChan GameState)

        return $ GameData { nhMap       = g2p
                          , naMap       = p2p
                          , acMap       = permToGo
                          , commandChan = cChan
                          , gameState   = gState }

sockHandler :: Handle -> GameData -> IO ()
sockHandler handle gData = do
    hSetBuffering handle LineBuffering
    forkIO $ commandProcessor handle gData
    return ()

commandProcessor :: Handle -> GameData -> IO ()
commandProcessor handle gData = do
   untilM_ (hIsEOF handle) handleCommand
   hClose handle
   return ()
  where
    handleCommand = do
       line <- hGetLine handle
       let pc@(cmd:arg) = words line
           nhMap_TMVar  = nhMap gData
           naMap_TMVar  = naMap gData
       naMap' <- atomically $ readTMVar naMap_TMVar
       hPutStrLn handle $ "naMap is " ++ (show naMap') ++ "\n"
       hPutStrLn handle ("command was " ++ (unwords pc))
       case cmd of
        "echo"  -> echoCommand handle arg
        "join"  -> joinGame handle arg nhMap_TMVar
        "list"  -> listPlayers handle nhMap_TMVar
        "start" -> gameManager gData
        _       -> doCommand gData handle $ maybeCommand pc naMap'
                    where
                      maybeCommand :: [String]     ->
                                      [(Name,AID)] ->
                                      (Maybe Command,String)
                      maybeCommand pc'@(verb:obj:_) naMap'' =
                         case verb of
                            "Zap"  -> (Just $ Zap aid,unwords pc')
                                      where
                                        aid = lookupJustNote fnh name naMap''
                                        name = Name (pack obj)
                                        fnh = "naMAP couldn't find " ++
                                              "the AID for "         ++
                                              obj                    ++
                                              "\n"
                            "Buy"  -> (readMay $
                                       unwords pc' :: (Maybe Command),unwords pc')
                            "Sell" -> (readMay $
                                       unwords  pc' :: (Maybe Command),unwords pc')
                            "Move" -> (readMay $
                                       unwords pc' :: (Maybe Command),unwords pc')
                            "SetSpeed" -> (readMay $
                                          unwords pc' :: (Maybe Command),unwords pc)
                            _      -> (Nothing,"Some weird shit happened\n")
                      maybeCommand (verb:[]) _ =
                         (readMay $ verb :: (Maybe Command),(unwords pc))
                      maybeCommand [] _ = (Nothing,"empty list")

doCommand :: GameData -> Handle -> (Maybe Command,String) -> IO ()
doCommand (GameData {nhMap = nhMap_TMVar
                    ,naMap = naMap_TMVar
                    ,commandChan = cChan_TChan})
          handle 
          ((Just cmd),str) = do
   nhMap' <- atomically $ readTMVar nhMap_TMVar
   naMap' <- atomically $ readTMVar naMap_TMVar
   hPutStrLn handle ("successful command" ++ str)
   atomically $ writeTChan cChan_TChan $ mkCommand nhMap' naMap' cmd
   where
     mkCommand :: [(Name,Handle)] -> [(Name,AID)] -> Command -> [UAC]
     mkCommand nhMap' naMap' cmd' =
        [uac]
        where
           uac = UAC $ PlayerCommand cmd' aid
           aid = lookupJustNote doCommandAIDFail name naMap'
           name = lookupJustNote doCommandNHFail handle hnMap
           hnMap = map swap nhMap'
           doCommandAIDFail = "doCommand failed to find the aid mapped to " ++
                              (show name)                 ++
                              "in naMap"
           doCommandNHFail  = "doCommand failed to find the name mapped to " ++
                              (show handle)                                  ++
                              "in hnMap"

doCommand _ handle (Nothing,str) =
   hPutStrLn handle ("That command made no sense " ++ str ++ " \n")

gameManager :: GameData -> IO ()
gameManager gData@(GameData {nhMap = nhMap_TMVar
                            ,naMap = naMap_TMVar
                            ,acMap = acMap_TMVar
                            ,commandChan = cChan_TChan
                            ,gameState = gState_TChan}) = do
   nhMap' <- atomically $ readTMVar nhMap_TMVar
   let aids         = map (AID . pack . show) [100 .. numOfPlayers]
       numOfPlayers = ((length nhMap') -1) + 100
       naMap'       = zip names aids
       names        = map fst nhMap'
       newMaps      = makeMaps aids naMap'

   announceStart nhMap'
   atomically $ swapTMVar naMap_TMVar naMap'
   atomically $ swapTMVar acMap_TMVar $ acMap_ $ aMap $ newMaps
   gameloop cChan_TChan gState_TChan newMaps
   forkIO $ responseManager gData
   return ()
   where
     acMap_ :: AgentMap -> [(AID,Bool)]
     acMap_ (AgentMap aMap') =
        map (allowCommand . fst) aMap'
        where
          allowCommand :: AID -> (AID,Bool)
          allowCommand aid = (aid,True)

     makeMaps :: [AID] -> [(Name,AID)] -> InitMaps
     makeMaps aids naMap' =
        InitMaps {aMap  = initAmap naMap'
                 ,pMap  = initPmap aids
                 ,lMaps = initLmaps aids }

responseManager :: GameData -> IO ()
responseManager (GameData {nhMap     = nhMap_TMVar
                          ,gameState = gState_TChan}) = forever $ do
   (GameState aMap' _) <- atomically $ readTChan gState_TChan
--   appendFile "rManager.txt" $ show gState
   sendResponse aMap' nhMap_TMVar
   return ()
   where
     sendResponse :: AgentMap              ->
                     TMVar [(Name,Handle)] ->
                     IO ()
     sendResponse (AgentMap aMap') nhMap_TMVar' = do
        nhMap' <- atomically $ readTMVar nhMap_TMVar'
        mapM_ sendResponse' $ map (prepResponse nhMap') $ mapMaybe filterDead aMap'
        where
          filterDead :: (AID,Agent) -> Maybe (AID,Agent)
          filterDead (_,(Dead _)) = Nothing
          filterDead agt      = Just agt

          prepResponse :: [(Name,Handle)] -> (AID,Agent) -> (Handle,[Message])
          prepResponse nhMap' (_, agt) =
             let (name,messages) = case agt of
                                    (Player _ _ _ _ _ _) ->
                                      (getName agt,getMessage agt)
                                    (Dead name') ->
                                      (name',[GameOver])
                 handle          = getHandle name
             in (handle ,messages)
             where
               getHandle name = lookupJustNote handleFail name nhMap'
                  where
                    handleFail   = "prepResponse couldn't find "  ++
                                   (show name)                    ++
                                   " \n"

          sendResponse' :: (Handle,[Message]) -> IO ()
          sendResponse' (handle, messages) = do
             mapM_ (hPutStrLn handle) $ map show messages

joinGame :: Handle                ->
            [String]              ->
            TMVar [(Name,Handle)] ->
            IO ()
joinGame handle (name:_) nhMap_TMVar = do
   nhMap' <- atomically $ takeTMVar nhMap_TMVar
   _ <- atomically                      $
        putTMVar nhMap_TMVar            $
        addToAL nhMap' (Name (pack name)) handle
   hPutStrLn handle ("you have joined the game as " ++
                    (show name)                     ++
                    "\n")
joinGame handle _ _ = do
   hPutStrLn handle ("usage: join <name>\n");

listPlayers :: Handle -> TMVar [(Name,Handle)] -> IO ()
listPlayers handle nhMap_TMVar = do
   nhMap' <- atomically $ takeTMVar nhMap_TMVar
   hPutStrLn handle $ show $ map (fromName . fst) nhMap'
   atomically $ putTMVar nhMap_TMVar nhMap'

fromName :: Name -> Text
fromName (Name name) = name

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle arg = do
    hPutStrLn handle (unwords arg)

announceStart :: [(Name,Handle)] -> IO ()
announceStart nhMap' = do
   _ <- mapM_ (announce message) nhMap'
   return ()
   where
     message = ", someone just started shit. Game on\n"


announce :: String -> (Name,Handle) -> IO ()
announce message ((Name name),handle) = do
   hPutStrLn handle $ (unpack name) ++ ", " ++ message

--allowCommand :: AgentMap -> [(AID,Bool)]
--allowCommand (AgentMap aMap) =
--   map (allowCommand' . fst) aMap
--   where
--     allowCommand' :: AID -> (AID,Bool)
--     allowCommand' aid = (aid,True)

untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ cond action = do
   b <- cond
   if b
     then return ()
     else action >> untilM_ cond action

port :: Int
port = 3000

