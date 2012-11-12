module Auxiliar where
import Scanner
---

f1 a b = "#include" ++ " <iostream.h>" ++ "\n"
         ++ "#include" ++ " <stdLib.h>"  ++ "\n"
         ++ "#include" ++ " <stdio.h>"  ++ "\n"
         ++ a ++ "\n" ++ b
f2 b = "int main" ++" () "++ "\n"
       ++ b ++ "\n"



j1 nom d a b = "import java.util.Scanner;" ++ "\n"
             ++ "public class " ++ nom ++ "\n" ++ "{"  ++ "\n" ++
        d ++ "\n" ++ a ++ "\n" ++ b ++ "\n" ++ "}"

j2 b =  "public void main() " ++ "\n"
       ++ b ++ "\n"

quit (x:xs) | x =='\"' = quit xs
            | otherwise = x:[] ++ quit xs
quit [] = []
