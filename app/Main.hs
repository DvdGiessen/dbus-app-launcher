{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Chan
import           Control.Exception.Extra (ignore)
import           Control.Monad
import           Data.Either
import           Data.List (uncons)
import           Data.Map.Strict (fromList, toList)
import           Data.Maybe (fromMaybe)
import           DBus.Client
import           ShellWords (parse)
import           System.Environment (getEnvironment)
import           System.Exit
import           System.Posix.Process
import           Text.Regex.TDFA

splitEnv :: [String] -> ([String], [String])
splitEnv = span (\ s -> ((s =~ ("[a-zA-Z_][a-zA-Z0-9_]*=" :: String)) :: Bool))

parseEnv :: [String] -> [(String, String)]
parseEnv = map ((\ (k, _, v) -> (k, v)) . (\ d -> ((d =~ ("=" :: String)) :: (String, String, String))) )

uniqEnv :: [[(String, String)]] -> [(String, String)]
uniqEnv envs = toList (fromList (concat envs))

parseCmd :: String -> [String] -> [(String, String)] -> (String, [String], [(String, String)])
parseCmd cmd args env = (\ (a, b)
   -> uncurry
        (,,) (fromMaybe (cmd, args) (uncons (b ++ args)))
        (uniqEnv [parseEnv a, env])) (splitEnv (fromRight [] (parse cmd)))

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
               [ autoMethod "Cmd" (\ cmd -> writeChan channel (parseCmd cmd [] []))
               , autoMethod "CmdArgs" (\ cmd args -> writeChan channel (parseCmd cmd args []))
               , autoMethod "CmdArgsEnv" (\ cmd args env -> writeChan channel (parseCmd cmd args (toList env)))
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

    -- Environment that spawned processes will inherit
    globalEnv <- getEnvironment

    -- Exec with the requested parameters
    _ <- executeFile cmd True args (Just (uniqEnv [globalEnv, env]))

    -- Never reached
    return ()
