build:
	idris forth.idr -o foid
.PHONY: build 

clean:
	rm foid
.PHONY: clean 
