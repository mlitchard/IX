module Main where
import IX.Server.Server
import Control.Concurrent (forkIO)

main :: IO ()
main = do 
   server 6666
   return ()
