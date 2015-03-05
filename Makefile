all: connect4

connect4: ConnectFour.hs
	ghc ConnectFour.hs -o connect4

clean:
	rm -f *.o *.hi connect4

again: clean all