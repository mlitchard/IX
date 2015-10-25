{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module IX.Server.Server
  (server)
  where

import           Debug.Trace
import           DataStructures.Composite
import           DataStructures.Atomic
import           IX.Universe.Input
import           IX.Reactive.EventNetwork (gameloop)
import           Conduit
import           Safe (readMay,fromJustNote)
import           Data.Conduit
import           Data.Functor ((<$>))
import           Data.Conduit.TMChan
import           Control.Concurrent.STM
import qualified Data.Map.Strict       as M
import qualified Data.List             as L
import qualified Data.Text             as T
import           Control.Monad.IO.Class
import           Data.Conduit.Network
import qualified Data.ByteString.Char8 as BS
import           Text.Printf              (printf)
import           Data.Word8               (_cr)
import           Control.Monad
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (concurrently)
import           Control.Exception        (finally)

server :: Int -> IO ()
server port' = do
  server <- newServer
  runTCPServer (serverSettings port' "*") $ \app -> do
    (fromClient, client) <-
      appSource app $$+ readName server app `fuseUpstream` appSink app
    print (clientName client)
    (runClient fromClient server client) `finally` (removeClient server client)

clientSink :: Client -> Sink BS.ByteString IO ()
clientSink Client{..} = mapC SCommand =$ sinkTMChan clientChan True

newServer :: IO Server
newServer = do
  cs  <- newTVarIO M.empty
  cns <- newTVarIO M.empty
  gs <- newEmptyTMVarIO :: IO (TMVar GameState)
  cc  <- newTChanIO
  go  <- newTVarIO False
  return 
    Server { 
       clients_TVar      = cs
     , clientNames_TVar  = cns -- client sode mapping names to aid
     , gameState_TMVar   = gs
     , commandChan_TChan = cc
     , gameon_TVar       = go
    }

readName :: Server -> AppData -> ConduitM BS.ByteString BS.ByteString IO Client
readName server app = go
  where
  go = do
    yield "What is your game name? " $$ appSink app
    name <- lineAsciiC $ takeCE 80 =$= filterCE (/= _cr) =$= foldC
    if BS.null name then
      go
    else do
      ok <- liftIO $ checkAddClient server name app
      case ok of
        Nothing -> do
          respond "The name '%s' is in use, please choose another\n" name
          go
        Just client -> do
          respond "Welcome, %s!\n" name
          return client
  respond msg name = yield $ BS.pack $ printf msg $ BS.unpack name

checkAddClient :: Server -> ClientName -> AppData -> IO (Maybe Client)
checkAddClient server@Server{..} name app = atomically $ do
  clientmap <- readTVar clients_TVar
  if M.member name clientmap then
    return Nothing
  else do
    camap  <- readTVar clientNames_TVar -- maps aid to name
    let aid = AID (T.pack $ show $ (((M.size camap) -1) + 100))
    client <- newClient name app
    writeTVar clients_TVar (M.insert name client clientmap)
    writeTVar clientNames_TVar (M.insert name aid camap)
    let c_msg = BS.pack (show aid ++ " has connected")
    broadcast server  (Notice (name <++> c_msg))
    return (Just client)

broadcast :: Server -> SMessage -> STM ()
broadcast Server{..} msg = do
  clientmap <- readTVar clients_TVar
  mapM_ (\client -> sendMessage client msg) (M.elems clientmap)

runClient :: ResumableSource IO BS.ByteString -> Server -> Client -> IO ()
runClient clientSource server client@Client{..} =
  void $ concurrently
  (clientSource $$+- linesUnboundedAsciiC =$ clientSink client)
  (sourceTMChan clientChan $$ handleMessage server client =$ appSink clientApp)

removeClient :: Server -> Client -> IO ()
removeClient server@Server{..} client@Client{..} = atomically $ do
  client_names <- (readTVar clientNames_TVar)
  let aid = fromJustNote clientFail (M.lookup clientName client_names)
  modifyTVar' clients_TVar (M.delete clientName)
  modifyTVar' clientNames_TVar (M.filter (== aid))
  broadcast server $ Notice (clientName <++> " has disconnected")
  where
    clientFail = "removeClient failed to find aid " ++
                 "which should have been player "   ++
                 (BS.unpack clientName)                

sendMessage :: Client -> SMessage -> STM ()
sendMessage Client{..} msg = writeTMChan clientChan msg

listClients :: Server -> STM [ClientName]
listClients Server{..} = do
  c <- readTVar clients_TVar
  return $ M.keys c

newClient :: ClientName -> AppData -> STM Client
newClient name app = do
  chan <- newTMChan
  return Client { clientName     = name
                , clientApp      = app
                , clientChan     = chan
                }

sendToName :: Server -> ClientName -> SMessage -> STM Bool
sendToName server@Server{..} name msg = do
  clientmap <- readTVar clients_TVar
  case M.lookup name clientmap of
      Nothing -> return False
      Just client -> sendMessage client msg >> return True

handleMessage :: Server -> Client -> Conduit SMessage IO BS.ByteString
handleMessage server client@Client{..} = awaitForever $ \case
  Notice msg         -> output $ "*** " <++> msg
  Tell name msg      -> output $ "*" <++> name <++> "*: " <++> msg
  Broadcast name msg -> output $ "<" <++> name <++> ">: " <++> msg
  SCommand msg       -> case BS.words msg of
    ["/start"] -> do
      ok <- liftIO (gameManager server)
      if ok then output ("Starting Game") else output ("Game already started") 
    
    ["/tell", who, what] -> do
      ok <- liftIO $ atomically $ sendToName server who $ Tell clientName what
      unless ok $ output $ who <++> " is not connected."
    ["/help"] ->
      mapM_ output [ "------ help -----"
                   , "/start - starts a new game"
                   , "/shout <message> - Everyone needs to know eh?"
                   , "/tell <who> <what> - send a private message"
                   , "/list - list users online"
                   , "/help - show this message"
                   , "/quit - leave"
                   ]
    ["/list"] -> do
      cl <- liftIO $ atomically $ listClients server
      output $ BS.concat $ "----- online -----\n" : map ((flip BS.snoc) '\n') cl
    ["/quit"] -> do
      error . BS.unpack $ clientName <++> " has quit"
    ["/shout"] -> do
      liftIO $ atomically $ broadcast server $ Broadcast clientName msg
        -- ignore empty strings
    [""] -> return ()
    [] -> return ()

        -- broadcasts
    ws ->
      if BS.head (head ws) == '/' then
        output $ "Unrecognized command: " <++> msg
      else do
        res <- liftIO                $
               atomically            $
               commandManager server $
               GCommand clientName msg
        output $  BS.pack ("Game Command Debug: " ++ res)
        return ()
  where
    output s = yield (s <++> "\n")

(<++>) = BS.append

gameManager server@Server{..} = do
  gameStarted <- atomically (readTVar gameon_TVar)
  anMap       <- atomically (readTVar clientNames_TVar)
  let aids = M.elems anMap
  if gameStarted == True then
    return False -- code smell
  else do
    let anMap_keys = M.keys anMap
        initMaps   =
          InitMaps {
              aMap = initAmap anMap  
            , pMap = initPmap aids
            , lMap = initLmap aids
          } 
    _ <- (gameloop commandChan_TChan gameState_TMVar initMaps)
    _ <- atomically (swapTVar gameon_TVar True) 
    forkIO $ atomically (responseManager server)
    return True

commandManager Server{..} (GCommand c_name msg) = 
  let read = (readMay $ BS.unpack msg) :: Maybe Command
  in case read of
       Just command -> 
         do
            c_map <- readTVar clientNames_TVar
            let 
                aid = fromJustNote aidFail (M.lookup c_name c_map)
                uac = UAC (PlayerCommand command aid)
            writeTChan commandChan_TChan uac
            return $ show command
            where
              aidFail = "comandManager could not find " ++
                        (show c_name)                   ++
                        "in client map"
       Nothing      -> return "Invalid Command"
      

responseManager :: Server -> STM ()
responseManager server@Server{..} = forever $ do
  (GameState (AgentMap a_map) _) <- (takeTMVar gameState_TMVar)
  let s_messages = M.elems $ M.map mkMsgMap a_map
  mapM_ (response server) s_messages
  
  return ()

response :: Server -> (ClientName,SMessage) -> STM ()
response server (c_name,s_msg) = do
  _ <- sendToName server c_name s_msg
  traceShowM (show s_msg)
  return () 
  
mkMsgMap :: Agent -> (ClientName,SMessage)
mkMsgMap Player{..} =
  let msg_ = GTell aName $ BS.pack $ unlines (map show msg)
  in (aName,msg_)
