all:
	ghc mines.hs -o mines
clear:
	rm mines & rm mines.hi & rm mines.o
