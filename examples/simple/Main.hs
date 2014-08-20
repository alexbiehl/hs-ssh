
module Main where

import SSH
import SSH.Channel (defaultChannelConfig)
import SSH.Session (scKeyPair, defaultSessionConfig)
import SSH.Crypto  (rsaKeyPairFromFile)
import Network.Socket.Internal (PortNumber(..))

main :: IO ()
main = do
  keypair <- rsaKeyPairFromFile "id_rsa"

  start (defaultSessionConfig { scKeyPair = keypair })
        defaultChannelConfig (PortNum 22)
