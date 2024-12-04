module Core where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

letra_pertenece :: Char -> String -> Bool
letra_pertenece letra "" = False
letra_pertenece letra (p : palabra) =
  if letra == p
    then True
    else letra_pertenece letra palabra

letra_lugar_correcto :: Char -> Char -> Bool
letra_lugar_correcto letra1 letra2 = letra1 == letra2

match' :: String -> String -> String -> [(Char, Match)]
match' (p1 : palabra1) (p2 : palabra2) palabra_inicial =
  if letra_pertenece p1 palabra_inicial
    then
      if letra_lugar_correcto p1 p2
        then
          (p1, Correcto) : match' palabra1 palabra2 palabra_inicial
        else
          (p1, LugarIncorrecto) : match' palabra1 palabra2 palabra_inicial
    else
      (p1, NoPertenece) : match' palabra1 palabra2 palabra_inicial
match' _ _ _ = []

match :: String -> String -> [(Char, Match)]
match palabra1 palabra2 = match' palabra1 palabra2 palabra1

-- >>> match "posta" "seria"
-- [('p',LugarIncorrecto),('o',LugarIncorrecto),('s',LugarIncorrecto),('t',LugarIncorrecto),('a',Correcto)]

-- >>> match "posta" "posta"
-- [('p',Correcto),('o',Correcto),('s',Correcto),('t',Correcto),('a',Correcto)]
