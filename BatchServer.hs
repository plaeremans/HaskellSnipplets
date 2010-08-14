module Main where 
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Network
import System.Process
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 
import Random 
import Data.Map
import Control.Exception
import Prelude hiding (catch)


main = do  
  socket <- listenOn $ PortNumber 8899 
  sd <- initServerData  
  forkIO $ serverLoop sd socket
  finally (batchProcessor sd)  (putStrLn "exited :-(!!!!!!")

serverLoop :: ServerData -> Socket -> IO ()
serverLoop sd socket =   do
                    (handle, _, _) <- accept socket                    
                    forkIO $ handleRequest  sd handle
                    serverLoop sd socket


type  Ticket = MD5Digest

data JobStatus = Processed | InQueue | Busy | Failed Int deriving (Eq, Show)
data JobInfo = JobInfo {inputFile :: FilePath, outputFile :: FilePath, ticket :: Ticket, status :: JobStatus  }   deriving (Eq, Show)
data ServerData =  ServerData { ticket2JobInfo :: MVar (Map Ticket JobInfo), numberGenerator :: StdGen, jobQueue :: Chan JobInfo} 

newJob inputFile outputFile  ticket = JobInfo inputFile outputFile ticket InQueue

initServerData :: IO ServerData 
initServerData = do 
  gen <- newStdGen 
  mvm <- newMVar empty
  chan <- newChan 
  return $ ServerData  mvm gen chan
  

--randOmFileName :: StdGen -> (String, StdGen)
randomFileName g = let (r, ng) = next g in (((show r) ++ ".txt"), ng)


createJobInfo ticket = do 
  gen <- newStdGen 
  let (inputFileName, ng) = randomFileName gen
  let (outputFileName, _) = randomFileName ng
  return $ newJob inputFileName outputFileName ticket

queueJob :: ServerData -> JobInfo -> IO  ()
queueJob sd jobInfo = writeChan  (jobQueue sd)  jobInfo
  
handleCommand :: ServerData -> String -> Handle -> IO()
handleCommand sd "process" handle =   do  
  size <- hGetLine handle
  putStrLn $ size
  cts <- B.hGet handle (read size :: Int) 
  hClose handle 
  let md5cts = md5 cts
  jobInfo <- createJobInfo md5cts
  B.writeFile (inputFile jobInfo) cts
  queueJob sd jobInfo 
  putStrLn "Finished"  
  putStrLn $ show jobInfo
  return ()

handleCommand sd "fetchResult" handle = return ()
handleCommand sd "queryBusy" handle = return ()  
handleCommand sd "delete" handle = return ()

handleRequest :: ServerData -> Handle -> IO ()
handleRequest sd handle = do 
  command <- hGetLine handle
  handleCommand sd command handle

batchProcessor :: ServerData  -> IO ()
batchProcessor sd = do
  let chan = jobQueue sd
  let mvar = (ticket2JobInfo sd)
  jobInfo <- readChan chan
  putStrLn "going to process"
  updateJobStatus mvar jobInfo Busy
  ph <- runCommand ("sleep 10; cp " ++ (inputFile jobInfo) ++ " " ++ (outputFile jobInfo)) 
  putStrLn "forked process"
  exitCode <- waitForProcess ph
  putStrLn $ "Processed " ++ (show exitCode)
  let newState = case exitCode of 
                   ExitSuccess -> Processed
                   ExitFailure st  -> Failed st
  updateJobStatus mvar jobInfo  newState
  batchProcessor sd
  where updateJobStatus mvar jobInfo newStatus = do
                           mp <- takeMVar mvar 
                           putMVar mvar (insert (ticket jobInfo) (jobInfo {status = newStatus })  mp)