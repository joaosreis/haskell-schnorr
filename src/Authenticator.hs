module Authenticator (generateCertificate,generateSupplicantCert) where
    import Schnorr
    import FileOperations
    import NetworkOperations

    import System.IO
    import Crypto.Number.ModArithmetic
    import Crypto.Number.Generate
    import OpenSSL
    import OpenSSL.X509
    import OpenSSL.RSA
    import OpenSSL.PEM
    import OpenSSL.EVP.Digest
    import OpenSSL.EVP.Cipher
    import OpenSSL.EVP.Sign
    import OpenSSL.EVP.Verify
    import OpenSSL.EVP.Open
    import Data.Time.Clock
    import Data.Time.Calendar
    import Data.Time.LocalTime
    import Data.Fixed
    import qualified Data.ByteString.Char8 as C
    import Data.Maybe

    newCertificate :: IO (X509,RSAKeyPair)
    newCertificate = withOpenSSL $ do
        cert <- newX509
        setVersion cert 2
        (y,m,d) <- fmap (toGregorian . utctDay) getCurrentTime
        timezone <- getCurrentTimeZone
        (TimeOfDay hh mm ss) <- fmap (localTimeOfDay . utcToLocalTime timezone) getCurrentTime
        let serial = show y ++ show m ++ show d ++ show hh ++ show mm ++ show ss
        setSerialNumber cert 1--(read (filter (/= '.') serial) :: Integer)
        setIssuerName cert [("CN","Schnorr")]
        setSubjectName cert [("CN","Schnorr")]
        getCurrentTime >>= setNotBefore cert
        getCurrentTime >>= (setNotAfter cert . addUTCTime 788400000)
        keyPair <- generateRSAKey' 2048 65537
        setPublicKey cert keyPair
        getDigestByName "SHA256" >>= signX509 cert keyPair
        return (cert,keyPair)

    generateCertificate :: FilePath -> FilePath-> Bool -> IO ()
    generateCertificate certFilePath keyFilePath encrypt = do
        (cert,keyPair) <- newCertificate
        writeX509ToFile cert certFilePath
        writePKCS8PrivateKeyToFile keyPair keyFilePath encrypt

    generateSupplicantCert :: FilePath -> FilePath -> String -> IO ()
    generateSupplicantCert certFilePath keyFilePath port = withOpenSSL $ do
        h <- listenConnection port
        cert <- readX509FromFile certFilePath
        k <- readPrivateKeyFromFile keyFilePath
        encKeyToSign <- C.hGetLine h
        decK <- C.hGetLine h
        iv <- C.hGetLine h
        Just cipher <- getCipherByName "aes128"
        let keyToSign = openBS cipher decK iv k encKeyToSign
        C.putStrLn keyToSign
        Just digest <- getDigestByName "SHA256"
        putStrLn "gambi tudo fixe até aqui"

        signature <- signBS digest k keyToSign
        putStrLn "gambi"
        sendMessage h (ByteStringMessage keyToSign)
        putStrLn "gambi"
        sendMessage h (ByteStringMessage signature)
        putStrLn "gambi"
        hClose h
        putStrLn "gambiF"


    authenticate :: FilePath -> String -> IO ()
    authenticate pkFilePath port = do
        -- Read public key and parameters from file
        (v,params) <- readPublicKeyFromFile pkFilePath

        -- Start server and receive connection
        h <- listenConnection port

        -- Receive x
        xS <- hGetLine h
        let x = read xS :: Integer

        -- Calculate and send e = rand [0,2^t]
        e <- generateBetween 0 (2 ^ t params) :: IO Integer
        hPrint h e

        -- Receive y
        yS <- hGetLine h
        let y = read yS :: Integer

        -- Calculate z = (beta^y)*(v^e) mod P
        let z = mod (expSafe (beta params) y (p params) * expSafe v e (p params)) (p params)

        -- Compare x and z. If equal, authentication is successful
        if x == z
            then do
                sendMessage h (StringMessage "Authentication successful")
                putStrLn "Authentication successful"
            else do
                sendMessage h (StringMessage "Authentication unsuccessful")
                putStrLn "Authentication unsuccessful"

        hClose h
