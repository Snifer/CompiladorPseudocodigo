module Scanner where

import UU_Parsing

type Tokens = [Token]

data Token  = Token Tok String NumLin
type NumLin = Int

instance Show Token where
  show (Token t str nl) = show t ++ " " ++ show str ++ " en la linea " ++ show nl ++ "\n"

data Tok  = TokIdent
          | TokPalClave
          | TokOpClave
          | TokSimbolo
          | TokEntero
          | TokCadena
          | TokError
          deriving (Eq, Ord)

instance Show Tok where
  show TokPalClave = " Palabra clave : "
  show TokOpClave  = " Operador clave: "
  show TokIdent    = " Identificador : "
  show TokSimbolo  = " Simbolo      : "
  show TokEntero   = " Numero entero : "
  show TokCadena   = " Cadena : "
  show TokError    = " Error         : "

scanner :: PalsClave -> OpsClave -> OpSimbs -> OpBasic -> FileName -> IO Tokens
scanner psc osc oss bas fn = do tokens <- tokenize psc osc oss bas fn
                                return tokens

type FileName  = String
type PalsClave = [String]


tokenize psc osc oss bas fn = do input <- readFile fn
                                 let tokens = scan psc osc oss bas input 1
                                 return tokens

scan psc osc oss bas xs n = scan' xs n
  where scan' []         _ = []
        scan' xxs@(x:xs) n
          = if isSpaceComent x xs then scan' nbs nn else Token tok str n : scan' rs n
          where (tok,str,rs) = token x xs
                (nn,nbs)     = saltarBlancos xxs n

        isOperator x       = x `elem` bas
        isSimbolo x        = x `elem` oss
        isSpaceComent x xs = isSpace x || isComentario x xs

        palClave str
          | str `elem` psc = TokPalClave
          | otherwise      = TokIdent

        token x xs
          | isDigit x    = let (str,xss) = span isDigit    xs
                           in  (TokEntero   , x:str, xss)
          | isOperator x = let (str,xss) = span isOperator xs
                               nxs       = x:str
                           in  (opClave nxs , nxs  , xss)

          | isSimbolo x = (TokSimbolo , x:[]  , xs)
          | otherwise    = (TokError, "simbolo desconocido \"" ++ x:[] ++ "\"", xs)

saltarBlancos []     n = (n,[])
saltarBlancos xxs@(x:xs) n
  | isSpace x = saltarBlancos xs (n + if x == '\n' then 1 else 0)
  | (x == '/' && (head xs) == '/') = (n, (dropWhile (/='\n') xs))
  | otherwise = (n,xxs)

instance Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Token tok1 str1 _ <= Token tok2 str2 _
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token