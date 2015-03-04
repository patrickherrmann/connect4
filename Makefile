all: connect4

connect4:
	ghc ConnectFour.hs -o connect4

clean:
	rm -f *.o *.hi connect4

again: clean all