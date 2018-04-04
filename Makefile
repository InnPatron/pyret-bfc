.PHONY: clean


build:	tokens.arr
	pyret tokens.arr

clean:
	rm ./*.jarr
