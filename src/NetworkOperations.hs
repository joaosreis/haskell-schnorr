module NetworkOperations (listenConnection,openConnection,sendMessage,
Message(StringMessage,ByteStringMessage)) where
    import Network.Socket
    import System.IO
    import qualified Data.ByteString.Char8 as C

    data Message = StringMessage String | ByteStringMessage C.ByteString

    listenConnection :: String -> IO Handle
    listenConnection port = withSocketsDo $ do
        -- Look up the port.  Either raises an exception or returns
        -- a nonempty list.
        addrinfos <- getAddrInfo
            (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
            Nothing (Just port)
        let serveraddr = head addrinfos
        -- Create a socket
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol

        -- Bind it to the address we're listening to
        bindSocket sock (addrAddress serveraddr)

        -- Start listening for connection requests.  Maximum queue size
        -- of 5 connection requests waiting to be accepted.
        listen sock 1

        putStrLn "Waiting for connection..."

        -- Accept connection
        (connsock, _) <- accept sock

        -- Configure handle
        connhdl <- socketToHandle connsock ReadWriteMode
        -- hSetBuffering connhdl LineBuffering
        hSetBuffering connhdl (BlockBuffering Nothing)

        return connhdl


    openConnection :: HostName -> String -> IO Handle
    openConnection hostname port = withSocketsDo $ do
        -- Look up the hostname and port.  Either raises an exception
        -- or returns a nonempty list.  First element in that list
        -- is supposed to be the best option.
        addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
        let serveraddr = head addrinfos

        -- Establish a socket for communication
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol

        -- Mark the socket for keep-alive handling since it may be idle
        -- for long periods of time
        setSocketOption sock KeepAlive 1

        -- Connect to server
        connect sock (addrAddress serveraddr)

        -- Make a Handle out of it for convenience
        h <- socketToHandle sock ReadWriteMode

        -- We're going to set buffering to BlockBuffering and then
        -- explicitly call hFlush after each message, below, so that
        -- messages get logged immediately
        -- hSetBuffering h LineBuffering
        hSetBuffering h (BlockBuffering Nothing)

        -- Save off the socket, program name, and server address in a handle
        return h

    sendMessage :: Handle -> Message -> IO ()
    sendMessage h (StringMessage m) = withSocketsDo $ do
        hPutStrLn h m
        hFlush h
    sendMessage h (ByteStringMessage m) = withSocketsDo $ do
        C.hPutStrLn h m
        hFlush h
