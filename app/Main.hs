{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Chan
import           Control.Exception.Extra (ignore)
import           Control.Monad
import           DBus.Client
import           System.Exit
import           System.Posix.Process

main :: IO ()
main = do
    -- Set up session
    client <- connectSession

    -- Channel for transfering the process to launch from the callback thread
    channel <- newChan
    
    -- Export object used for launching programs
    export client "/" defaultInterface
             { interfaceName = "nl.dvdgiessen.dbusapplauncher.Exec"
             , interfaceMethods =
               [ autoMethod "Exec" (\ cmd -> (writeChan channel (cmd, [], Nothing))) ]
             }

    -- Connect session to bus
    requestResult <- requestName client "nl.dvdgiessen.dbusapplauncher" []
    when (requestResult /= NamePrimaryOwner) $ do
        putStrLn "Another service owns the \"nl.dvdgiessen.dbusapplauncher\" bus name"
        exitFailure

    -- Retrieve the command to launch from the callback
    (cmd, args, env) <- readChan channel

    -- Give the thread one millisecond to return its result
    threadDelay 1000

    -- Disconnect the session
    unexport client "/Launch"
    disconnect client

    -- Make sure we are in our own session
    ignore (do _ <- createSession; return ())

    -- Execute the received command
    _ <- executeFile cmd True args env

    -- Never reached
    return ()

