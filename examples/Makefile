ALL = PlayMIDI.js InteractivePiano.js basic.html
OPTS = --debug

all: $(ALL)

PlayMIDI.js: PlayMIDI.elm
	elm make $(OPTS) PlayMIDI.elm --output PlayMIDI.js

InteractivePiano.js: InteractivePiano.elm
	elm make $(OPTS) InteractivePiano.elm --output InteractivePiano.js

basic.html: Basic.elm
	elm make $(OPTS) Basic.elm --output basic.html

clean:
	rm -rf $(ALL) elm-stuff/
