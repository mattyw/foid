build:
	idris foid.idr -o foid
.PHONY: build 

clean:
	rm foid
.PHONY: clean 
