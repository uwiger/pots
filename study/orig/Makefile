
POTS = ../../ebin

all: control.beam

%.beam: %.erl
	erlc -W -I ../../src $<

run: control.beam
	(cd ../../src; make)
	erl -pa $(POTS) -pa . -eval 'start:it(sim)'

clean:
	rm -f *~ *.beam \#*\#
