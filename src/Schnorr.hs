module Schnorr (generateParameters,generateKeys,SecretKey,PublicKey,
    Keys(Keys,sk,pk),SchnorrParameters(SchnorrParameters,p,q,beta,t),
    Signature(Signature,k,signature)) where

    import Crypto.Number.Generate
    import Crypto.Number.Prime
    import Crypto.Number.ModArithmetic
    import Crypto.Number.Basic
    import qualified Data.ByteString.Char8 as C

    type SecretKey = Integer
    type PublicKey = Integer

    data SchnorrParameters = SchnorrParameters {
        p :: Integer,
        q :: Integer,
        beta :: Integer,
        t :: Integer
    } deriving (Eq,Show)

    data Keys = Keys { sk :: SecretKey, pk :: PublicKey } deriving (Eq,Show)

    data Signature = Signature { k :: C.ByteString, signature :: C.ByteString }
        deriving (Eq,Show)

    -- Find the primitive root of Z(P), with Q = (P-1)/r
    findPrimitiveRootModP :: Integer -> Integer -> Integer
    findPrimitiveRootModP p q = aux 1
        where
            r = div (p-1) 2
            aux i = if expSafe i q p /= 1 && expSafe i r p /= 1
                then i
                else aux (i+1)

    -- Generate beta = alfa^((P-1)/Q) mod P, with alfa = PrimitRoot(Z(P))
    -- and Q = (P-1)/r
    generateBeta :: Integer -> Integer -> Integer
    generateBeta p q = expSafe a (div (p-1) q) p
        where
            a = findPrimitiveRootModP p q


    generateParameters :: IO SchnorrParameters
    generateParameters = do
        -- Generates random prime P in the form 2*Q+1
        p <- generateSafePrime 1024 :: IO Integer
        let q = div (p - 1) 2
        let beta = generateBeta p q
        t <- generateMax (toInteger (numBits q)) -- t = rand [0,numBits Q]
        let params = SchnorrParameters {
            p = p,
            q = q,
            beta = beta,
            t = t
        }
        return params

    generateKeys :: SchnorrParameters -> IO Keys
    generateKeys (SchnorrParameters p q beta _) = do
        -- secret key <- a = rand [0, Q-1]
        a <- generateMax (q-1 :: Integer) :: IO Integer
        -- public key <- v = beta^(-a) mod P
        let Just invBeta = inverse beta p
        let v = expSafe invBeta a p
        let k = Keys {
            sk = a,
            pk = v
        }
        return k
