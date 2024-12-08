module Core (Match (..), match) where

data Match = Correcto | LugarIncorrecto | NoPertenece
  deriving (Eq, Show)

-- Función que compara el intento con la palabra secreta y devuelve la lista de coincidencias
match :: String -> String -> [(Char, Match)]
match palabraSecreta intento = zipWith determinarMatch intento [0 ..]
  where
    determinarMatch :: Char -> Int -> (Char, Match)
    determinarMatch c i
      | c == palabraSecreta !! i = (c, Correcto)
      | c `elem` palabraSecreta = (c, LugarIncorrecto)
      | otherwise = (c, NoPertenece)
