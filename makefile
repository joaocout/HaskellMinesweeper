all:
	ghc mapa.hs -o mapa
clear:
	rm mapa & rm mapa.hi &	rm mapa.o
