all:
	ghc -O3 AOC.hs -package parsec

clean:
	find . -name "*.o" -type f -delete
	find . -name "*.hi" -type f -delete
	rm AOC
