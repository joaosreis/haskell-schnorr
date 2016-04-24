module Authenticator (authenticate) where
    import Schnorr
    import FileOperations(readPublicKeyFromFile)
    import NetworkOperations(listenConnection)

    import System.IO
    import Crypto.Number.ModArithmetic
    import Crypto.Number.Generate

    authenticate :: FilePath -> String -> IO ()
    authenticate pkFilePath port = do
        -- Read public key and parameters from file
        (v,params) <- readPublicKeyFromFile pkFilePath

        -- Start server and receive connection
        (h, clientaddr) <- listenConnection port

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
                hPutStrLn h "Authentication successful"
                putStrLn "Authentication successful"
            else do
                hPutStrLn h "Authentication unsuccessful"
                putStrLn "Authentication unsuccessful"

        hClose h
