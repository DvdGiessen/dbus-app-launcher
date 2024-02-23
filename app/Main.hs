{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           System.Exit
import           System.Process
import           DBus.Client

launchProcess :: String -> IO ()
launchProcess cmd = do
    (_, _, _, handle) <- createProcess (proc cmd []){std_in = NoStream, std_out = NoStream, std_err = NoStream, close_fds = True, create_group = True, new_session = True}
    -- TODO: save handle, and then only start new instance after last one closed? also properly close handle so there's no zombie process?
    return ()

main :: IO ()
main = do
    -- Set up session
    client <- connectSession
    
    -- Export object used for launching programs
    export client "/Launch" defaultInterface
             { interfaceName = "nl.dvdgiessen.dbusapplauncher.LaunchProgram"
             , interfaceMethods =
               [ autoMethod "Foot" (launchProcess "/usr/bin/foot") ]
             }

    -- Connect session to bus
    requestResult <- requestName client "nl.dvdgiessen.dbusapplauncher" []
    when (requestResult /= NamePrimaryOwner) $ do
        putStrLn "Another service owns the \"nl.dvdgiessen.dbusapplauncher\" bus name"
        exitFailure

    -- Wait forever for method calls
    forever (threadDelay 50000)

