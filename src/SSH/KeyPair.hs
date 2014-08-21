{-# LANGUAGE OverloadedStrings #-}
module SSH.KeyPair (
    rsaKeyPairFromFile
  , dsaKeyPairFromFile
  , readRSAKeyPair
  , readDSAKeyPair
  ) where

import Data.ASN1.Types
import Data.ASN1.Encoding (decodeASN1)
import Data.ASN1.BinaryEncoding

import Crypto.Types.PubKey.RSA as RSA
import Crypto.Types.PubKey.DSA as DSA

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LBS

readASN1Object :: ASN1Object a => LBS.ByteString -> Either String a
readASN1Object asn1data = do
    (a, _) <- decode asn1data >>= fromASN1
    return a
  where
    decode s = case decodeASN1 BER s of
                Left err -> Left (show err)
                Right a  -> return a

readKeyPair :: ASN1Object a => LBS.ByteString -> Either String a
readKeyPair s = readASN1Object =<< decoded s
  where
    decoded  = LB64.decode
              . LBS.concat
              . filter (not . isDashed)
              . LBS.lines
    isDashed = LBS.isPrefixOf "--"

keypairFromFile :: FilePath
                -> (LBS.ByteString -> Either String a)
                -> IO (Either String a)
keypairFromFile path decode = do
  content <- LBS.readFile path
  return (decode content)

rsaKeyPairFromFile :: FilePath -> IO (Either String RSA.KeyPair)
rsaKeyPairFromFile path = keypairFromFile path readRSAKeyPair

dsaKeyPairFromFile :: FilePath -> IO (Either String DSA.KeyPair)
dsaKeyPairFromFile path = keypairFromFile path readDSAKeyPair

readRSAKeyPair :: LBS.ByteString -> Either String RSA.KeyPair
readRSAKeyPair = readKeyPair

readDSAKeyPair :: LBS.ByteString -> Either String DSA.KeyPair
readDSAKeyPair = readKeyPair
