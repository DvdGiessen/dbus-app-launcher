{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Chan
import           Control.Exception.Extra (ignore)
import           Control.Monad
import           Data.Map
import           DBus.Client
import           System.Exit
import           System.Posix.Process

main :: IO ()
main = do
    -- Connect to D-Bus
    client <- connectSession

    -- Channel for transfering exec parameters from the callback thread
    channel <- newChan
    
    -- Export object used for launching programs
    export client "/nl/dvdgiessen/DBusAppLauncher" defaultInterface
             { interfaceName = "nl.dvdgiessen.dbusapplauncher.Exec"
             , interfaceMethods =
               [ autoMethod "Cmd" (\ cmd -> (writeChan channel (cmd, [], Nothing)))
               , autoMethod "CmdArgs" (\ cmd args -> (writeChan channel (cmd, args, Nothing)))
               , autoMethod "CmdArgsEnv" (\ cmd args env -> (writeChan channel (cmd, args, Just (toList env))))
               ]
             }

    -- Register our service
    requestResult <- requestName client "nl.dvdgiessen.dbusapplauncher" []
    when (requestResult /= NamePrimaryOwner) $ do
        putStrLn "Another service owns the \"nl.dvdgiessen.dbusapplauncher\" bus name"
        exitFailure

    -- Wait for the callback thread to return the exec parameters
    (cmd, args, env) <- readChan channel

    -- Do not accept any additional calls
    unexport client "/nl/dvdgiessen/DBusAppLauncher"

    -- Give the callback thread one millisecond to return its result before we kill it
    threadDelay 1000

    -- Disconnect from D-Bus, killing the callback thread
    disconnect client

    -- Make sure we are in our own process group and session
    ignore (do _ <- createSession; return ())

    -- Exec with the requested parameters
    _ <- executeFile cmd True args env

    -- Never reached
    return ()

