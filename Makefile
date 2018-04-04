.PHONY: clean

check: tokens.arr bf.arr
	pyret -y tokens.arr bf.arr

build:	tokens.arr bf.arr
	pyret tokens.arr bf.arr

clean:
	rm ./*.jarr
