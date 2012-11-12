module Main where
import UU_Parsing
import Auxiliar
import Scanner
import ParserG
import Gra


palabrasClave = ["funcion","procedimiento","devolver","inicio","fin","entero"]


operadores = ["<-", "''","!=",">=","<="]
opeBadicos = "<'!-"
simbolos = "+-\";)(,:*."

main = do
       putStr "Nombre del Archivo: "
       nomF <- getLine
       tokens <-  scanner palabrasClave operadores simbolos opeBadicos (nomF++".seu")
       let errSintac = buscars tokens
       out <- parseIO pRoot tokens
       let (genC,dod,pas,nom,nps,ja,use,var) = out
       putStr (show tokens)

       let errSemNpF = repetidos nps
       putStr (show errSemNpF)
       let errSemNom = repetidos var
       let errSemUses = validUsadas var use
       putStr (show errSemNom)
       putStr (show errSemUses)
       if (validConstrr errSemNom errSemUses errSintac) then (generarC nomF gen) else mensajeError1 errSemNom errSemUses errSintac
       if (validConstrr [][][]) then (generarC nomF genC ja pas ) else mensajeError1 errSemNom errSemUses errSintac
       }

buscars (x:xs)|buss x==[]=buscars xs
              |otherwise=buss x:buscars xs
buscars []=[]

bus (Tok(TkError,a,b,c,d)) = (b++ " en la linea "++show(c))
bus _ = ""

buss::Token -> String

buss (Token TokError a b ) = (a ++ " en la LINEA " ++ show(b))
buss _ = ""