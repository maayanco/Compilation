all: example

example: dest.o
	gcc -o example dest.o

dest.o: dest.c
	gcc -c dest.c

clean:
	rm dest.o example

