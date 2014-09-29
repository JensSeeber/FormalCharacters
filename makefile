all: doc/manual.pdf

doc/manual.pdf: makedoc.g PackageInfo.g doc/*.xml gap/*.gd gap/*.gi examples/*.g
	gap makedoc.g

.PHONY: clean
clean:
	rm doc/manual.pdf
