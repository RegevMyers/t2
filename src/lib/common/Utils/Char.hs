module Utils.Char (
    toChar
) where

toChar :: Int -> Maybe Char
toChar int
    | (int >= 0 && int <= 25) = Just $ toEnum (fromEnum 'A' + int)
    | (otherwise)             = Nothing