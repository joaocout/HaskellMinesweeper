import System.Random
import Data.List

--matriz de duplas (first, second)
--first indica a qtd de bombas vizinhas a celula
--sendo que, se first == 9, entao a celula eh uma bomba
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

randompositions::StdGen -> StdGen -> Int -> Int -> [(Int,Int)]
randompositions gen1 gen2 qtd maxrange =
    zip (take qtd (randomRs (1,maxrange) gen1)) (take qtd (randomRs (1,maxrange) gen2))

adicionarbombas::[[(Int,Int)]] -> [(Int,Int)] -> Int -> Int -> [[(Int,Int)]]
adicionarbombas tabuleiro bombas linhaatual tamanho =
    if(length bombas > 0) then
        --ha bomba na linha atual
        if(snd(head bombas) == linhaatual) then
            adicionarbombas ([take (fst(head bombas)-1) (head tabuleiro) ++ [(9,0)] ++ drop (fst(head bombas)) (head tabuleiro)] ++ tail tabuleiro) (tail bombas) linhaatual tamanho
        --n ha mais bombas na linha atual
        else
            [head tabuleiro] ++ adicionarbombas (tail tabuleiro) bombas (succ linhaatual) tamanho 
    else
        tabuleiro


compare_snd::(Int,Int) -> (Int,Int) -> Ordering
compare_snd a b =
    if(snd a < snd b) then LT
    else GT

main::IO()
main = do
    g1 <- newStdGen
    g2 <- newStdGen
    let tamanho = 10
    let qtdbombas = 3
    let bombas = sortBy compare_snd (randompositions g1 g2 qtdbombas tamanho)
    let tabvazio = novomapa tamanho
    let tabcbomba = adicionarbombas tabvazio bombas 1 tamanho
    printmapa tabvazio tamanho
    printmapa tabcbomba tamanho
    print(bombas)