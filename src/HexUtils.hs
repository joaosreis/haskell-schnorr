module HexUtils (hexStringToInt,intToHexString) where
    import Data.Char(digitToInt)

    intToHexString :: Integer -> String
    intToHexString = aux ""
        where
            aux h 0 = h
            aux h d =
                let hi = mod d 16
                    c = case hi of
                        10 -> "a"
                        11 -> "b"
                        12 -> "c"
                        13 -> "d"
                        14 -> "e"
                        15 -> "f"
                        _ -> show hi
                in aux (c++h) (div (d-hi) 16)

    hexStringToInt :: String -> Integer
    hexStringToInt s = aux 0 1 (reverse s)
        where
            aux r _ "" = r
            aux r i (c:cs) = aux (r + toInteger(digitToInt c) * i) (i*16) cs
