novomapa n = [[0 | i<-[1..n]] | j<-[1..n]]

printprimeiralinha lista = 
    if(head lista == 1) then do
        putStr("     ")
        putStr(show (head lista) ++ " ")
        printprimeiralinha (tail lista)
    else if length lista > 1 then do
        putStr(show (head lista) ++ " ")
        printprimeiralinha (tail lista)
    else
        putStr(show(head lista) ++ "\n") 

printlinha linha = do
    if length linha > 0 then do
        putStr("# ")
        printlinha (tail linha)
    else
        putStr("\n")

printmapa' mapa n = do
    if length mapa > 0 then do
        if(n - length mapa + 1 < 10) then
            putStr(show ( n - (length mapa) + 1) ++ "  | ")
        else
            putStr(show ( n - (length mapa) + 1) ++ " | ")
        printlinha (head mapa)
        printmapa' (tail mapa) n
    else
        putStr("Escolha uma posicao (x,y): \n")

printmapa mapa n = do
    printprimeiralinha [1..n]
    printmapa' mapa n

main = do
    let tamanho = 10
    let m = novomapa tamanho
    printmapa m tamanho
