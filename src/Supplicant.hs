module Supplicant (genKeys,supplicantAuthenticate,signKey) where
    import Schnorr
    import FileOperations
    import NetworkOperations
    import HexUtils

    import System.IO
    import Crypto.Number.Generate
    import Crypto.Number.ModArithmetic
    import qualified Data.ByteString.Char8 as C
    import OpenSSL
    import OpenSSL.EVP.Seal
    import OpenSSL.EVP.Cipher
    import OpenSSL.X509
    import OpenSSL.RSA
    import OpenSSL.EVP.Digest
    import OpenSSL.EVP.Verify
    import Control.Monad



    genKeys :: FilePath -> FilePath -> IO ()
    genKeys skFilePath pkFilePath = do
        putStrLn "This may take over a minute, please wait..."
        params <- generateParameters
        k <- generateKeys params
        writeSecretKeyToFile params (sk k) skFilePath
        writePublicKeyToFile params (pk k) pkFilePath

    signKey :: FilePath -> FilePath -> FilePath -> String -> String -> IO ()
    signKey pkFilePath authCertFilePath generatedCertFilePath hostname port =
        withOpenSSL $ do
            h <- openConnection hostname port
            keyToSign <- C.readFile pkFilePath
            cert <- readX509FromFile authCertFilePath
            pk <- getPublicKey cert
            Just cipher <- getCipherByName "aes128"
            (encKey,[encK],iv) <- sealBS cipher [pk] keyToSign
            sendMessage h (ByteStringMessage encKey)
            sendMessage h (ByteStringMessage encK)
            sendMessage h (ByteStringMessage iv)
            keySigned <- C.hGetContents h
            putStrLn "gambi tudo fixe até aqui"

            signature <- C.hGetLine h
            putStrLn "gambi"
            when (keySigned /= keyToSign) (error "Error in communication")
            putStrLn "gambi"
            Just digest <- getDigestByName "SHA256"
            putStrLn "gambi"
            status <- verifyBS digest signature pk keyToSign
            putStrLn "gambi"
            when (status == VerifyFailure) (error "Signature not valid")
            putStrLn "gambi"
            let sig = Signature keyToSign signature
            putStrLn "gambi"
            writeSignatureToFile sig generatedCertFilePath
            putStrLn "gambi"
            hClose h
            putStrLn "gambiF"

    supplicantAuthenticate :: FilePath -> String -> String -> IO ()
    supplicantAuthenticate skFilePath hostname port = do
        (a,params) <- readSecretKeyFromFile skFilePath

        h <- openConnection hostname port

        r <- generateBetween 0 (q params - 1) :: IO Integer
        let x = expSafe (beta params) r (p params)

        hPrint h x

        eS <- hGetLine h
        let e = read eS :: Integer

        when (e > 2 ^ t params) (error "Invalid e")

        let yy = mod (a * e + r) (q params)

        hPrint h yy

        result <- hGetLine h

        putStrLn result

        hClose h
