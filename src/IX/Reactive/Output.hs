module IX.Reactive.Output
   (writeOut)
   where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

writeOut gsChannel gameOutPut = 
   atomically (putTMVar gsChannel gameOutPut)
