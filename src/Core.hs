module Core where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

-- Verifica si una letra pertenece a una palabra
letraPertenece :: Char -> String -> Bool
letraPertenece letra "" = False
letraPertenece letra (p : palabra) =
  if letra == p
    then True
    else letraPertenece letra palabra

-- Verifica si dos letras están en la posición correcta
letraLugarCorrecto :: Char -> Char -> Bool
letraLugarCorrecto letra1 letra2 = letra1 == letra2

-- Función auxiliar para realizar el "match"
match' :: String -> String -> String -> [(Char, Match)]
match' (p1 : palabra1) (p2 : palabra2) palabraInicial
  | letraLugarCorrecto p1 p2 = (p2, Correcto) : match' palabra1 palabra2 palabraInicial
  | letraPertenece p2 palabraInicial = (p2, LugarIncorrecto) : match' palabra1 palabra2 palabraInicial
  | otherwise = (p2, NoPertenece) : match' palabra1 palabra2 palabraInicial
match' _ _ _ = [] -- Caso base: listas vacías

-- Función principal que llama a match'
match :: String -> String -> [(Char, Match)]
match palabra1 palabra2 = match' palabra1 palabra2 palabra1

-- >>> match "aspas" "popas"
-- [('p',LugarIncorrecto),('o',NoPertenece),('p',Correcto),('a',Correcto),('s',Correcto)]

-- >>> match "posta" "nonon"
-- [('n',NoPertenece),('o',Correcto),('n',NoPertenece),('o',LugarIncorrecto),('n',NoPertenece)]
