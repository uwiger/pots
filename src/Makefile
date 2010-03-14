

.SUFFIXES: .erl .beam

EBIN = ../ebin

SOURCES := $(wildcard *.erl)
MODULES := $(patsubst %.erl,%,$(SOURCES))
BEAMS := $(patsubst %,$(EBIN)/%.beam,$(MODULES))

vpath %.beam $(EBIN)

ERLC = erlc -W -o $(EBIN)

all: $(BEAMS)

clean:
	rm -f *~ $(EBIN)/*.beam

../ebin/%.beam: %.erl
	$(ERLC) $<

