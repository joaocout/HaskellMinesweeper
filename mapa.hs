import System.Random
import Data.List


--limpa a tela do ghci
clear::IO()
clear = do
    putStr("\ESC[2J")


--matriz de duplas (first, second)
--first indica a qtd de bombas vizinhas a celula
--sendo que, se first == 9, entao a celula eh uma bomba
--second indica a visibilidade das celulas, se == 0 (coberta), se == 1 (descoberta)
novomapa::Int -> [[(Int,Int)]]
novomapa n = [[(0,1) | i<-[1..n]] | j<-[1..n]]


--imprime a primeira linha do mapa (coordenada)
--no formato (1 2 3 4 5 6 ...)
printprimeiralinha::[Int] -> IO()
printprimeiralinha lista = 
    if(head lista == 1) then do
        putStr("     ")
        putStr(show (head lista) ++ " ")
        printprimeiralinha (tail lista)
    else if length lista > 1 then do
        putStr(show (head lista) ++ " ")
        printprimeiralinha (tail lista)
    else
        putStr(show (head lista) ++ "\n") 


--imprime cada celula da linha que recebe
--a celula pode ser '#', quando ainda nao revelada
--ou pode ser um numero indicativo de qts bombas vizinhas ha
printlinha::[(Int,Int)] -> IO()
printlinha linha = do
    if length linha > 0 then do
        if snd (head linha) == 0 then putStr("# ")
        else putStr(show (fst(head linha)) ++ " ")
        printlinha (tail linha)
    else
        putStr("\n")


--funcao auxiliar de impressao do tabuleiro
printmapa'::[[(Int,Int)]] -> Int -> IO()
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


--impressao do tabuleiro
printmapa::[[(Int,Int)]] -> Int -> IO()
printmapa mapa n = do
    printprimeiralinha [1..n]
    printmapa' mapa n


--gera uma lista de posicoes randomicas para as bombas
randompositions::StdGen -> StdGen -> Int -> Int -> [(Int,Int)]
randompositions gen1 gen2 qtd maxrange =
    zip (take qtd (randomRs (1,maxrange) gen1)) (take qtd (randomRs (1,maxrange) gen2))


--adiciona as bombas ao mapa
adicionarbombas::[[(Int,Int)]] -> [(Int,Int)] -> Int -> [[(Int,Int)]]
adicionarbombas tabuleiro bombas linhaatual =
    if(length bombas > 0) then
        --ha bomba na linha atual
        if(snd(head bombas) == linhaatual) then
            adicionarbombas ([take (fst(head bombas)-1) (head tabuleiro) ++ [(9,snd $ head $ drop (fst(head bombas)-1) (head tabuleiro))] ++ drop (fst(head bombas)) (head tabuleiro)] ++ tail tabuleiro) (tail bombas) linhaatual
        --n ha mais bombas na linha atual
        else
            [head tabuleiro] ++ adicionarbombas (tail tabuleiro) bombas (succ linhaatual) 
    else
        tabuleiro


--compara o segundo termo da dupla
compare_snd::(Int,Int) -> (Int,Int) -> Ordering
compare_snd a b =
    if(snd a < snd b) then LT
    else GT


--retorna a quantidade de bombas vizinhas a celula
updatecelula::(Int, Int) -> [(Int,Int)] -> Int
updatecelula posicaoatual bombas = do
    let x = fst posicaoatual
    let y = snd posicaoatual
    let vizinhos = [(x+1,y+1), (x+1,y-1), (x+1, y), (x-1,y+1), (x-1,y-1), (x-1, y), (x, y+1), (x, y-1)]
    if length bombas > 0 then
        if(elem (head bombas) vizinhos) then
            1+updatecelula posicaoatual (tail bombas)
        else
            updatecelula posicaoatual (tail bombas)
    else 0

    
--auxilia a updatemapa a adicionar os valores ao mapa
updatelinha::[(Int,Int)] -> [(Int,Int)] -> Int -> Int -> Int -> [(Int,Int)]
updatelinha linha bombas tamanho linhaatual colunaatual =
    if(colunaatual < tamanho) then
        if fst(head linha) /= 9 then
            [(fst(head linha)+(updatecelula (colunaatual, linhaatual) bombas), snd (head linha))] ++ updatelinha (tail linha) bombas tamanho linhaatual (succ colunaatual) 
        else
            [(head linha)] ++ updatelinha (tail linha) bombas tamanho linhaatual (succ colunaatual)
    else
        [(fst(head linha)+(updatecelula (colunaatual, linhaatual) bombas), snd (head linha))]


--adiciona todos os valores restantes do mapa (numeros indicativos de qtd de bombas vizinhas)
updatemapa::[[(Int,Int)]] -> [(Int,Int)] -> Int -> Int -> [[(Int,Int)]]
updatemapa tab bombas tamanho linhaatual = 
    if(linhaatual < tamanho) then
        [updatelinha (head tab) bombas tamanho linhaatual 1] ++ updatemapa (tail tab) bombas tamanho (succ linhaatual)
    else
        [updatelinha (head tab) bombas tamanho linhaatual 1]


--loop principal do jogo, finaliza quando a flag fim é true
game::[[(Int,Int)]] -> [(Int,Int)] -> Int -> Bool -> IO()
game tab bombas tamanho fim = do
    if(fim == True) then do
        clear
        print("fim de jogo")
        return()
    else do
        clear
        print("bombas = " ++ show(bombas))
        printmapa tab tamanho
        input <- getLine
        let jogada = (read input :: (Int,Int))
        if(elem jogada bombas) then game tab bombas tamanho True
        else game tab bombas tamanho False


main::IO()
main = do
    g1 <- newStdGen
    g2 <- newStdGen
    let tamanho = 10
    let qtdbombas = 3
    let bombas = sortBy compare_snd (randompositions g1 g2 qtdbombas tamanho)
    let tabvazio = novomapa tamanho
    let tabcbomba = adicionarbombas tabvazio bombas 1
    let tab = updatemapa tabcbomba bombas tamanho 1
    print(bombas)
    printmapa tab tamanho