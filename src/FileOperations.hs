module FileOperations (writePublicKeyToFile,writeSecretKeyToFile,
    readPublicKeyFromFile,readSecretKeyFromFile,writeX509ToFile,
    readX509FromFile,writePKCS8PrivateKeyToFile,readPrivateKeyFromFile,
    writeSignatureToFile,readSignatureFromFile) where

    import Schnorr as S
    import HexUtils

    import Data.List(intercalate)
    import System.Directory(createDirectoryIfMissing)
    import Filesystem.Path.CurrentOS(parent,decodeString,encodeString)
    import Data.Maybe
    import OpenSSL.X509
    import OpenSSL.PEM
    import OpenSSL.RSA
    import OpenSSL.EVP.Cipher
    import OpenSSL.EVP.PKey
    import OpenSSL
    import qualified Data.ByteString.Char8 as C

    writePublicKeyToFile :: SchnorrParameters -> S.PublicKey -> FilePath -> IO ()
    writePublicKeyToFile params pk pkFilePath = do
        let parentDir = encodeString (parent (decodeString pkFilePath))
        createDirectoryIfMissing True parentDir
        let pList = [p params,q params,beta params,t params]
        let pkString = intercalate "\n" (map intToHexString (pList ++ [pk]))
        writeFile pkFilePath pkString

    writeSecretKeyToFile :: SchnorrParameters -> SecretKey -> FilePath -> IO ()
    writeSecretKeyToFile params sk skFilePath = do
        let parentDir = encodeString (parent (decodeString skFilePath))
        createDirectoryIfMissing True parentDir
        let pList = [p params,q params,beta params,t params]
        let skString = intercalate "\n" (map intToHexString (pList ++ [sk]))
        writeFile skFilePath skString

    readPublicKeyFromFile :: FilePath -> IO (S.PublicKey,SchnorrParameters)
    readPublicKeyFromFile pkFilePath = do
        file <- readFile pkFilePath
        let pS:qS:bS:tS:pkS:xs = lines file
        let params = SchnorrParameters {
            p = hexStringToInt pS,
            q = hexStringToInt qS,
            beta = hexStringToInt bS,
            t = hexStringToInt tS
        }
        let pk = hexStringToInt pkS
        return (pk,params)

    readSecretKeyFromFile :: FilePath -> IO (SecretKey,SchnorrParameters)
    readSecretKeyFromFile skFilePath = do
        file <- readFile skFilePath
        let pS:qS:bS:tS:skS:xs = lines file
        let params = SchnorrParameters {
            p = hexStringToInt pS,
            q = hexStringToInt qS,
            beta = hexStringToInt bS,
            t = hexStringToInt tS
        }
        let sk = hexStringToInt skS
        return (sk,params)

    writeX509ToFile :: X509 -> FilePath -> IO ()
    writeX509ToFile cert certFilePath = withOpenSSL $ do
        let parentDir = encodeString (parent (decodeString certFilePath))
        createDirectoryIfMissing True parentDir
        writeX509 cert >>= writeFile certFilePath

    readX509FromFile :: FilePath -> IO X509
    readX509FromFile certFilePath = withOpenSSL $
        readFile certFilePath >>= readX509

    writePKCS8PrivateKeyToFile :: RSAKeyPair -> FilePath -> Bool -> IO ()
    writePKCS8PrivateKeyToFile keyPair keyFilePath encrypt = withOpenSSL $ do
        let parentDir = encodeString (parent (decodeString keyFilePath))
        createDirectoryIfMissing True parentDir
        Just cipher <- getCipherByName "AES-128-CBC"
        if encrypt
            then writePKCS8PrivateKey keyPair (Just (cipher,PwTTY)) >>= writeFile keyFilePath
            else writePKCS8PrivateKey keyPair (Just (cipher,PwNone)) >>= writeFile keyFilePath

    readPrivateKeyFromFile :: FilePath -> IO RSAKeyPair
    readPrivateKeyFromFile keyFilePath = withOpenSSL $
        fmap (fromJust . toKeyPair) (readFile keyFilePath >>= (`readPrivateKey` PwTTY))

    writeSignatureToFile :: Signature -> FilePath -> IO ()
    writeSignatureToFile sig sigFilePath = do
        let parentDir = encodeString (parent (decodeString sigFilePath))
        createDirectoryIfMissing True parentDir
        C.writeFile sigFilePath $ C.concat [k sig,C.pack "\n",signature sig]

    readSignatureFromFile :: FilePath -> IO Signature
    readSignatureFromFile sigFilePath = do
        file <- C.readFile sigFilePath
        let k:sig:xs = C.lines file
        return (Signature k sig)
