.PHONY: clean


build:	tokens.arr bf.arr
	pyret tokens.arr bf.arr

clean:
	rm ./*.jarr
