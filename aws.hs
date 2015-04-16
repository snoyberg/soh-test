{-# LANGUAGE OverloadedStrings #-}
import Network.AWS
import Network.AWS.ECS
import Network.AWS.EC2
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe
import Data.Text (unpack)
import Control.Concurrent
import Data.Map (Map, singleton, empty, lookup)
import Prelude hiding (lookup)

runWarp = "arn:aws:ecs:us-east-1:105814010004:task-definition/run-warp-server:2"

getRight m = do
    r <- m
    case r of
        Left e -> error $ show e
        Right x -> return x

killAllTasks e = do
    r <- getRight $ send e $ listTasks

    mapM_ (send e . stopTask) $ r^.ltrTaskArns

describeWhenRunning e taskArns =
    loop (10 :: Int) 100000
  where
    loop 0 _ = error $ "describeWhenRunning: took too long for " ++ show taskArns
    loop cnt delay = do
        r <- getRight $ send e $ set dtTasks taskArns describeTasks
        let statuses = flip toListOf r $ dtrTasks.folded.tLastStatus
        case statuses of
            [Just "RUNNING"] -> return r
            [Just "PENDING"] -> do
                liftIO $ do
                    putStrLn "Still pending"
                    threadDelay delay
                loop (cnt - 1) $ min (delay * 2) 1000000
            _ -> error $ "Unexpected statuses: " ++ show statuses

data PortInfo = PortInfo
    { cmdPort :: !Int
    , userPort :: !Int
    }

main :: IO ()
main = runResourceT $ do
    e <- liftIO $ getEnv NorthVirginia Discover

    killAllTasks e

    r <- getRight $ send e $ runTask "soh-test"

    let taskArns = flip toListOf r $ rtrTasks.folded.tTaskArn.folded

    r <- describeWhenRunning e taskArns

    let ports :: Map Int Int
        ports = r ^. (dtrTasks.folded.tContainers.folded.cNetworkBindings.folded.folding toMap)
        toMap :: NetworkBinding -> Maybe (Map Int Int)
        toMap nb = do
            cp <- nb ^. nbContainerPort
            hp <- nb ^. nbHostPort
            return $ singleton cp hp

    let cinsts = toListOf (dtrTasks.folded.tContainerInstanceArn.folded) r
    r <- getRight $ send e $ set dciContainerInstances cinsts describeContainerInstances

    let ec2s = toListOf (dcirContainerInstances.folded.ciEc2InstanceId.folded) r

    r <- getRight $ send e $ set di1InstanceIds ec2s describeInstances
    let hosts = toListOf (dirReservations.folded.rInstances.folded.i1PublicIpAddress.folded) r

    liftIO $ case hosts of
        [host] ->
            case lookup 5000 ports of
                Nothing -> error "Port 5000 not found"
                Just p ->
                    case lookup 5001 ports of
                        Nothing -> error "Port 5001 not found"
                        Just up -> do
                            putStrLn $ "Command URL: http://" ++ unpack host ++ ':' : show p
                            putStrLn $ "User URL   : http://" ++ unpack host ++ ':' : show up
        _ -> error $ "Invalid hosts: " ++ show hosts
