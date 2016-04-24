module Test (test1,test2) where
    import Schnorr
    import FileOperations
    import Authenticator
    import Supplicant

    import Control.Concurrent

    pkFilePath = "public_test.key"
    skFilePath = "secret_test.key"
    hostname = "127.0.0.1"
    port = "5050"

    test1 = do
        params <- generateParameters
        k <- generateKeys params
        writePublicKeyToFile params (pk k) pkFilePath
        writeSecretKeyToFile params (sk k) skFilePath
        (pk2,params2) <- readPublicKeyFromFile pkFilePath
        putStr "PK Test: "
        print (pk2 == pk k)
        print (params2 == params)
        (sk2,params2) <- readPublicKeyFromFile skFilePath
        putStr "SK Test: "
        print (sk2 == sk k)
        print (params2 == params)

    test2 = do
        genKeys skFilePath pkFilePath
        forkIO $ (\f -> do
            threadDelay 1000000
            f skFilePath hostname port) supplicantAuthenticate
        authenticate pkFilePath port
