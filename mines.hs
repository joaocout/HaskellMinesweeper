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
novomapa n = [[(0,0) | i<-[1..n]] | j<-[1..n]]


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
        else 
            if(fst (head linha) /= 0) then
                putStr(show (fst(head linha)) ++ " ")
            else
                putStr("  ")
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


--seta a celula escolhida como visivel
setarvisivel::[[(Int,Int)]] -> (Int,Int) -> [(Int,Int)] -> Int -> [[(Int,Int)]]
setarvisivel tab posicao vizinhos tamanho = do
    let x = fst posicao
    let y = snd posicao
    let linha = last  (take y tab)
    let aux = drop (x-1) linha
    let newtab = take (y-1) tab ++ [(take (x-1) linha) ++ [(fst(head aux), 1)] ++ (tail aux)] ++ drop y tab

    if(fst (tab !! (y-1) !! (x-1)) == 0 && snd (tab !! (y-1) !! (x-1)) == 0) then do newtab
        --let newvizinhos = getvizinhos newtab (head vizinhos) tamanho
        --setarvisivel newtab (head vizinhos) (union (tail(vizinhos)) newvizinhos) tamanho
    else
        newtab


--conta quantas celulas ainda nao foram reveladas na linha
contarnaoreveladoslinha::[(Int,Int)] -> Int
contarnaoreveladoslinha linha =
     length (filter (==0) (map snd linha))


--conta quantas celulas ainda nao foram reveladas no mapa
contarnaorevelados::[[(Int,Int)]] -> Int
contarnaorevelados tab = 
    if (length tab > 1) then
        contarnaoreveladoslinha (head tab) + contarnaorevelados (tail tab)
    else
        contarnaoreveladoslinha (head tab)


--checa se uma posicao nao foi descoberta
posicaovalida2::[[(Int,Int)]] -> (Int,Int) -> Bool
posicaovalida2 tab xy = do
    let x = fst xy
    let y = snd xy
    snd(tab !! (y-1) !! (x-1)) == 0 

--checa se uma posicao esta nos limites do mapa
posicaovalida::Int -> (Int,Int) -> Bool
posicaovalida tamanho xy = do
    let x = fst xy
    let y = snd xy
    x >=1 && y>=1 && x<=tamanho && y<=tamanho

--retorna os vizinhos nao descobertos de uma celula
getvizinhos::[[(Int,Int)]] -> (Int,Int) -> Int -> [(Int,Int)]
getvizinhos tab posicao tamanho = do
    let x = fst posicao
    let y = snd posicao
    let v = [(x+1,y+1), (x+1,y-1), (x+1, y), (x-1,y+1), (x-1,y-1), (x-1, y), (x, y+1), (x, y-1)]
    filter (posicaovalida tamanho) v

--loop principal do jogo, finaliza quando a flag fim é true
game::[[(Int,Int)]] -> [(Int,Int)] -> Int -> Bool -> Bool -> IO()
game tab bombas tamanho fim vitoria = do
    if(fim == True) then do
        clear
        print("fim de jogo")
        if(vitoria == True) then do
            print("vc venceu")
        else do
            print("vc perdeu")
        return()
    else do
        --verifica se todas as celulas que n sao bombas ja foram descobertas
        --se sim, vitoria
        if(contarnaorevelados tab == length bombas) then
            game tab bombas tamanho True True
        else do
            clear
            print ("bombas = " ++ show(bombas))
            printmapa tab tamanho
            input <- getLine
            let jogada = (read input :: (Int,Int))

            --se o player escolheu uma posicao de bomba, derrota
            if(elem jogada bombas) then
                game tab bombas tamanho True False
            
            --se nao, o jogo continua
            else do 
                game (setarvisivel tab jogada (getvizinhos tab jogada tamanho) tamanho) bombas tamanho False False


main::IO()
main = do
    g1 <- newStdGen
    g2 <- newStdGen
    let tamanho = 20
    let qtdbombas = 40
    let bombas = sortBy compare_snd (randompositions g1 g2 qtdbombas tamanho)
    let tabvazio = novomapa tamanho
    let tabcbomba = adicionarbombas tabvazio bombas 1
    let tab = updatemapa tabcbomba bombas tamanho 1
    game tab bombas tamanho False False