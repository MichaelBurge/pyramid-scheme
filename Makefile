all:
	find . -name '*.rkt' | xargs raco make

clean:
	find . -name 'compiled' | xargs rm -rf
