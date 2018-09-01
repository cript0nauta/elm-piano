OPTS = --debug
EXAMPLES = examples/PlayMIDI.js examples/InteractivePiano.js examples/basic.html
ALL = $(EXAMPLES) docs

all: $(ALL)

docs: documentation.json

examples: $(EXAMPLES)

examples/PlayMIDI.js: examples/PlayMIDI.elm
	cd examples && elm make $(OPTS) PlayMIDI.elm --output PlayMIDI.js

examples/InteractivePiano.js: examples/InteractivePiano.elm
	cd examples && elm make $(OPTS) InteractivePiano.elm --output InteractivePiano.js

examples/basic.html: examples/Basic.elm
	cd examples && elm make $(OPTS) Basic.elm --output basic.html

documentation.json: src/Piano.elm
	elm make --docs=documentation.json --output=/dev/null src/Piano.elm

clean:
	cd examples && rm -rf $(ALL) elm-stuff/
	rm -rf documentation.json
