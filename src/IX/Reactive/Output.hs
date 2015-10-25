module IX.Reactive.Output
   (writeOut
   ,writeOut_Debug)
   where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

writeOut gsChannel gameOutPut = 
  atomically (putTMVar gsChannel gameOutPut)

writeOut_Debug gsChannel gameOutPut =
  appendFile "eventOut.txt" $ show gameOutPut
