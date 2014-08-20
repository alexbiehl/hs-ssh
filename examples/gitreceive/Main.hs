{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import SSH
import SSH.Channel
import SSH.Session (scKeyPair, defaultSessionConfig)
import SSH.Crypto  (rsaKeyPairFromFile)
import Network.Socket.Internal (PortNumber(..))

import Control.Monad.State (gets)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad
import Control.Concurrent.MVar

import System.Exit
import System.Process hiding (spawnProcess)
import System.FilePath
import System.Directory
import System.Console.CmdArgs

data GitReceive = GitReceive {
    grCacheLock :: MVar ()
  , grRepoCache :: FilePath
  , grHookTempl :: String
}

data Options = Options {
    port     :: Int
  , cache    :: FilePath
  , noAuth   :: Bool
  , keys     :: FilePath
  , receiver :: FilePath
  } deriving (Show, Data, Typeable)

options = Options {
    port      = 22           &= help "port to listen on"                 &= opt (22 :: Int)
  , cache     = "/tmp/repos" &= help "path to repo cache"                &= opt "/tmp/repos"
  , noAuth    = False        &= help "disable client authentication"
  , keys      = ""           &= help "pem file containing private keys"  &= opt ""
  , receiver  = ""           &= help "command to execute on push"
  }

hookTmpl receiver = unlines [
  "#!/bin/bash",
  "set -eo pipefail; while read oldrev newrev refname; do",
  "[[ $refname = \"refs/heads/master\" ]] && git archive $newrev | "
     ++ receiver ++
      " \"$RECEIVE_REPO\" \"$newrev\" | sed -$([[ $(uname) == \"Darwin\" ]] && echo l || echo u) \"s/^/\"$'\\e[1G\\e[K'\"/\"",
  "done"
  ]

initGitReceive :: Options -> IO GitReceive
initGitReceive opts = do
  cacheLock <- newMVar ()
  return GitReceive {
    grCacheLock = cacheLock
  , grRepoCache = (cache opts)
  , grHookTempl = hookTmpl (receiver opts)
  }

data GitReceiveExc = GitReceiveExc String
  deriving (Show, Typeable)

instance Exception GitReceiveExc

extractRepo :: String -> Channel FilePath
extractRepo command = case (words command) of
    (cmd:arg:[]) | cmd == "git-receive-pack" -> return $ normalize (dropExtension arg)
                 | otherwise                 -> throwM (GitReceiveExc "Only `git push` is supported.")
    _                                        -> throwM (GitReceiveExc "Invalid arguments.")
  where
    normalize = dropWhile ('/'==) . dropWhile ('/'/=)

ensureCacheRepo :: GitReceive -> String -> IO ()
ensureCacheRepo gitReceive repo = bracket_ lock unlock $ do
    let repoPath   = (grRepoCache gitReceive </> repo)
    repoExists <- doesDirectoryExist repoPath

    unless repoExists $ do
      createDirectoryIfMissing True repoPath
      process  <- runProcess
                            "git"
                            ["init", "--bare"]
                            (Just repoPath)
                            Nothing
                            Nothing
                            Nothing
                            Nothing
      exitCode <- waitForProcess process

      case exitCode of
        ExitSuccess     -> do
          let hookFile = (repoPath </> "hooks" </> "pre-receive")
          writeFile
            hookFile
            (grHookTempl gitReceive)

          setPermissions hookFile $
            emptyPermissions { readable = True, executable = True}

        (ExitFailure _) -> throwM (GitReceiveExc "ensureCacheRepo")

    return ()
  where
    lock    = takeMVar (grCacheLock gitReceive)
    unlock  = putMVar (grCacheLock gitReceive) ()

channelHandler :: GitReceive -> Bool -> ChannelRequest -> Channel ()
channelHandler gr wr req = do
    channelHandler' gr wr req `catch` gitReceiveExcHandler
  where
    gitReceiveExcHandler (GitReceiveExc msg) = do
      channelError msg
      when wr channelFail

channelHandler' :: GitReceive -> Bool -> ChannelRequest -> Channel ()
channelHandler' gitReceive wantReply (Execute command) = do
  user     <- gets csUser
  repo     <- extractRepo command
  liftIO $ ensureCacheRepo gitReceive repo

  let process = runInteractiveProcess
                  "git-shell"
                  ["-c", "git-receive-pack '" ++ repo ++ "'"]
                  (Just $ grRepoCache gitReceive)
                  (Just [
                         ("RECEIVE_USER", user),
                         ("RECEIVE_REPO", repo)
                        ])

  spawnProcess process
  return ()

channelHandler' _ wr (Environment key value) = do
  when wr channelSuccess

channelHandler' _ _ request = throwM (GitReceiveExc $ "Can't handle command: " ++ show request)

main :: IO ()
main = do
  opts        <- cmdArgs options
  keypair     <- rsaKeyPairFromFile (keys opts)
  gitReceive  <- initGitReceive opts

  putStrLn (show opts)

  let channelConf = ChannelConfig (channelHandler gitReceive)
      sessionConf = defaultSessionConfig { scKeyPair = keypair }
      portNum     = PortNum $ fromIntegral (port opts)

  start sessionConf channelConf portNum

