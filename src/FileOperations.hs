module FileOperations (writePublicKeyToFile,writeSecretKeyToFile,
    readPublicKeyFromFile,readSecretKeyFromFile) where

    import Schnorr
    import HexUtils

    import Data.List(intercalate)
    import System.Directory(createDirectoryIfMissing)
    import Filesystem.Path.CurrentOS(parent,decodeString,encodeString)

    writePublicKeyToFile :: SchnorrParameters -> PublicKey -> FilePath -> IO ()
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

    readPublicKeyFromFile :: FilePath -> IO (PublicKey,SchnorrParameters)
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
