module Main (main) where
    import Schnorr
    import Supplicant
    import Authenticator
    import HexUtils
    import Options.Applicative
    import Control.Applicative

    data Command =
        GenerateKeys { sk :: String, pk :: String } |
        RequestSignKey {
            hostname :: String, port :: Maybe String,
            pk :: String, cert :: String, genCert :: String } |
        Supplicant { hostname :: String, port :: Maybe String, sk :: String } |
        GenerateCert { cert :: String, k :: String, encrypt :: Bool } |
        SignKey { port :: Maybe String, cert :: String, sk :: String } |
        Authenticator { port :: Maybe String, cert :: String, pk :: String }

    defaultPort = "5050"

    generateKeysParser :: Parser Command
    generateKeysParser = GenerateKeys
        <$> strOption
            (long "privkey"
            <> short 'k'
            <> metavar "FILE"
            <> help "File where to write the private key")
        <*> strOption
            (long "pubkey"
            <> short 'p'
            <> metavar "FILE"
            <> help "File where to write the public key")

    requestSignKeyParser :: Parser Command
    requestSignKeyParser = RequestSignKey
        <$> argument str
            (metavar "HOST")
        <*> optional (strOption
            (long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to connect"))
        <*> strOption
            (long "pubkey"
            <> short 'k'
            <> metavar "FILE"
            <> help "File containing the public key to sign")
        <*> strOption
            (long "auth-cert"
            <> metavar "FILE"
            <> help "File containing the authenticator certificate")
        <*> strOption
            (long "cert"
            <> short 'c'
            <> metavar "FILE"
            <> help "File where to write the certificate")

    supplicantParser :: Parser Command
    supplicantParser = Supplicant
        <$> argument str
            (metavar "HOST")
        <*> optional (strOption
            (long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to connect"))
        <*> strOption
            (long "key"
            <> short 'k'
            <> metavar "FILE"
            <> help "File containing the private key to authenticate")

    generateCertParser :: Parser Command
    generateCertParser = GenerateCert
        <$> strOption
            (long "cert"
            <> short 'c'
            <> metavar "FILE"
            <> help "File where to write the certificate")
        <*> strOption
            (long "key"
            <> short 'k'
            <> metavar "FILE"
            <> help "File where to write the private key")
        <*> switch
            (long "password"
            <> short 'p'
            <> help "Encrypt the resulting file")

    signKeyParser :: Parser Command
    signKeyParser = SignKey
        <$> optional (strOption
            (long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to connect"))
        <*> strOption
            (long "cert"
            <> short 'c'
            <> metavar "FILE"
            <> help "File containing the certificate")
        <*> strOption
            (long "key"
            <> short 'k'
            <> metavar "FILE"
            <> help "File containing the private key")

    authenticatorParser :: Parser Command
    authenticatorParser = Authenticator
        <$> optional (strOption
            (long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to use"))
        <*> strOption
            (long "cert"
            <> short 'c'
            <> metavar "FILE"
            <> help "File containing the certificate")
        <*> strOption
            (long "pubkey"
            <> short 'k'
            <> metavar "FILE"
            <> help "File containing the public key to authenticate")

    parser :: Parser Command
    parser = subparser
        (command "generate-keys" (info (helper <*> generateKeysParser)
            (progDesc "Generate a key-pair"))
        <> command "request-sign-key" (info (helper <*> requestSignKeyParser)
                (progDesc "Contact authenticator to sign a public key"))
        <> command "supplicant" (info (helper <*> supplicantParser)
            (progDesc "Use the supplicant mode"))
        <> command "generate-cert" (info (helper <*> generateCertParser)
            (progDesc "Generate an authenticator certificate"))
        <> command "sign-key" (info (helper <*> signKeyParser)
                (progDesc "Sign supplicant public key"))
        <> command "authenticator" (info (helper <*> authenticatorParser)
            (progDesc "Use the authenticator mode")))

    executeCommand :: Command -> IO ()
    executeCommand (GenerateKeys sk pk) = genKeys sk pk
    executeCommand (RequestSignKey hostname Nothing pk cert genCert) = signKey pk cert genCert hostname defaultPort
    executeCommand (RequestSignKey hostname (Just port) pk cert genCert) = signKey pk cert genCert hostname port
    executeCommand (Supplicant hostname Nothing sk) = supplicantAuthenticate sk hostname defaultPort
    executeCommand (Supplicant hostname (Just port) sk) = supplicantAuthenticate sk hostname port
    executeCommand (GenerateCert cert k encrypt) = generateCertificate cert k encrypt
    executeCommand (SignKey Nothing cert sk) = generateSupplicantCert cert sk defaultPort
    executeCommand (SignKey (Just port) cert sk) = generateSupplicantCert cert sk port
    -- executeCommand (Authenticator Nothing cert pk) = authenticate pk defaultPort
    -- executeCommand (Authenticator (Just port) cert pk) = authenticate pk port

    main :: IO ()
    main = do
        command <- execParser (info (helper <*> parser) idm)
        executeCommand command
        where
            idm = fullDesc
                <> progDesc "Authenticate or get authenticated using Schnorr protocol"
                <> header "Schnorr"
