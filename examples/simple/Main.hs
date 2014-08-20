
module Main where

import SSH
import SSH.Channel (defaultChannelConfig)
import SSH.Session (defaultSessionConfig)
import Network.Socket.Internal (PortNumber(..))

main :: IO ()
main = do
  start defaultSessionConfig defaultChannelConfig (PortNum 22)
