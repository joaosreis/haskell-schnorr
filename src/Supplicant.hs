module Supplicant (genKeys,supplicantAuthenticate) where
    import Schnorr
    import FileOperations
    import NetworkOperations(openConnection)

    import System.IO
    import Crypto.Number.Generate
    import Crypto.Number.ModArithmetic

    genKeys :: FilePath -> FilePath -> IO ()
    genKeys skFilePath pkFilePath = do
        putStrLn "This may take over a minute, please wait..."
        params <- generateParameters
        k <- generateKeys params
        writeSecretKeyToFile params (sk k) skFilePath
        writePublicKeyToFile params (pk k) pkFilePath

    supplicantAuthenticate :: FilePath -> String -> String -> IO ()
    supplicantAuthenticate skFilePath hostname port = do
        (a,params) <- readSecretKeyFromFile skFilePath

        h <- openConnection hostname port

        r <- generateBetween 0 (q params - 1) :: IO Integer
        let x = expSafe (beta params) r (p params)

        hPrint h x

        eS <- hGetLine h
        let e = read eS :: Integer

        -- TODO verify e

        let yy = mod (a * e + r) (q params)

        hPrint h yy

        result <- hGetLine h

        putStrLn result

        hClose h
