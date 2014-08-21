module SSH.Crypto where

import qualified SSH.KeyPair as KP

import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.ASN1.BinaryEncoding (BER(..))
import Data.ASN1.Encoding (decodeASN1)
import Data.ASN1.Stream
import Data.List (isPrefixOf)
import qualified Codec.Binary.Base64.String as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified OpenSSL.DSA as DSA

import Crypto.Hash.SHA1 as SHA1
import Crypto.PubKey.HashDescr (hashDescrSHA1)
import qualified Crypto.PubKey.RSA.PKCS15 as RSA1
import qualified Crypto.Types.PubKey.RSA as RSA1

import qualified Crypto.Cipher as CIPH

import SSH.Packet
import SSH.NetReader
import SSH.Util

data Cipher =
    Cipher
        { cType :: CipherType
        , cMode :: CipherMode
        , cBlockSize :: Int
        , cKeySize :: Int
        }

data CipherType = AES
data CipherMode = CBC

data HMAC =
    HMAC
        { hDigestSize :: Int
        , hFunction :: LBS.ByteString -> LBS.ByteString
        }

data PublicKey
    = RSAPublicKey RSA1.PublicKey
    | DSAPublicKey
        { dpubP :: Integer
        , dpubQ :: Integer
        , dpubG :: Integer
        , dpubY :: Integer
        }
    deriving (Eq, Show)

data KeyPair
    = RSAKeyPair RSA1.KeyPair

    | DSAKeyPair
        { dprivPub :: PublicKey
        , dprivX :: Integer
        }
    deriving (Eq, Show)

defaultKeyPair :: KeyPair
defaultKeyPair = RSAKeyPair (RSA1.KeyPair defaultPrivateKey)
    where
        defaultPublicKey  = RSA1.PublicKey 0 0 0
        defaultPrivateKey = RSA1.PrivateKey defaultPublicKey 0 0 0 0 0 0

keyPairPublicKey :: KeyPair -> PublicKey
keyPairPublicKey (RSAKeyPair kp)    = RSAPublicKey (RSA1.toPublicKey kp)
keyPairPublicKey (DSAKeyPair x _)   = x

rsaKeyPairFromFile :: FilePath -> IO KeyPair
rsaKeyPairFromFile fn = do
    keyPair <- KP.rsaKeyPairFromFile fn
    return $ case keyPair of
        Left _  -> error "Could not read keypair from file"
        Right x -> RSAKeyPair x

generator :: Integer
generator = 2

safePrime :: Integer
safePrime = 179769313486231590770839156793787453197860296048756011706444423684197180216158519368947833795864925541502180565485980503646440548199239100050792877003355816639229553136239076508735759914822574862575007425302077447712589550957937778424442426617334727629299387668709205606050270810842907692932019128194467627007

toBlocks :: (Integral a) => a -> LBS.ByteString -> [LBS.ByteString]
toBlocks _ m | m == LBS.empty = []
toBlocks bs m = b : rest
  where
    b = LBS.take (fromIntegral bs) m
    rest = toBlocks bs (LBS.drop (fromIntegral bs) m)

fromBlocks :: [LBS.ByteString] -> LBS.ByteString
fromBlocks = LBS.concat

modexp :: Integer -> Integer -> Integer -> Integer
modexp = modexp' 1
  where
    modexp' y _ 0 _ = y
    modexp' y z e n
        | e `mod` 2 == 1 = modexp' (y * z `mod` n) ((z ^ (2 :: Integer)) `mod` n) (e `div` 2) n
        | otherwise = modexp' y ((z ^ (2 :: Integer)) `mod` n) (e `div` 2) n

blob :: PublicKey -> LBS.ByteString
blob (RSAPublicKey publicKey) = doPacket $ do
    string "ssh-rsa"
    integer (RSA1.public_e publicKey)
    integer (RSA1.public_n publicKey)
blob (DSAPublicKey p q g y) = doPacket $ do
    string "ssh-dss"
    integer p
    integer q
    integer g
    integer y

blobToKey :: LBS.ByteString -> PublicKey
blobToKey s = flip evalState s $ do
    t <- readString

    case t of
        "ssh-rsa" -> do
            e <- readInteger
            n <- readInteger
            return $ RSAPublicKey (RSA1.PublicKey 0 n e)
        "ssh-dss" -> do
            [p, q, g, y] <- replicateM 4 readInteger
            return $ DSAPublicKey p q g y
        u -> error $ "unknown public key format: " ++ u


aes128cbcEncrypt :: BS.ByteString -> BS.ByteString -> LBS.ByteString -> LBS.ByteString
aes128cbcEncrypt key ivRaw = LBS.fromStrict . CIPH.cbcEncrypt (initAES128 key) iv . LBS.toStrict
    where
        initAES128 :: BS.ByteString -> CIPH.AES128
        initAES128 = either (error . show) CIPH.cipherInit . CIPH.makeKey

        iv = maybe (error "invalid IV") id $ CIPH.makeIV ivRaw

aes128cbcDecrypt :: BS.ByteString -> BS.ByteString -> LBS.ByteString -> LBS.ByteString
aes128cbcDecrypt key ivRaw = LBS.fromStrict . CIPH.cbcDecrypt (initAES128 key) iv . LBS.toStrict
    where
        initAES128 :: BS.ByteString -> CIPH.AES128
        initAES128 = either (error . show) CIPH.cipherInit . CIPH.makeKey

        iv = maybe (error "invalid IV") id $ CIPH.makeIV ivRaw



sign :: KeyPair -> LBS.ByteString -> IO LBS.ByteString
sign (RSAKeyPair kp) m = return $ LBS.concat
    [ netString "ssh-rsa"
    , netLBS (LBS.fromStrict signature)
    ]
    where
        signature = case RSA1.sign Nothing hashDescrSHA1 (RSA1.toPrivateKey kp) (LBS.toStrict m) of
            Left err -> error $ "error while signing message: " ++ show err
            Right x  -> x

sign (DSAKeyPair (DSAPublicKey p q g y) x) m = do
    (r, s) <- DSA.signDigestedDataWithDSA (DSA.tupleToDSAKeyPair (p, q, g, y, x)) digest
    return $ LBS.concat
        [ netString "ssh-dss"
        , netLBS $ LBS.concat
            [ LBS.pack $ i2osp 20 r
            , LBS.pack $ i2osp 20 s
            ]
        ]
  where
    digest = SHA1.hashlazy m
sign _ _ = error "sign: invalid key pair"
