OPTS = --debug
EXAMPLES = examples/PlayMIDI.js examples/InteractivePiano.js examples/basic.html
ALL = $(EXAMPLES) docs
MODULE_FILES = src/Piano.elm src/Piano/TouchEvents.elm

all: $(ALL)

docs: documentation.json

examples: $(EXAMPLES)

examples/PlayMIDI.js: examples/PlayMIDI.elm $(MODULE_FILES)
	cd examples && elm make $(OPTS) PlayMIDI.elm --output PlayMIDI.js

examples/InteractivePiano.js: examples/InteractivePiano.elm $(MODULE_FILES)
	cd examples && elm make $(OPTS) InteractivePiano.elm --output InteractivePiano.js

examples/basic.html: examples/Basic.elm $(MODULE_FILES)
	cd examples && elm make $(OPTS) Basic.elm --output basic.html

documentation.json: $(MODULE_FILES)
	elm make --docs=documentation.json --output=/dev/null src/Piano.elm

src/Piano.elm:
	elm make --output=/dev/null src/Piano.elm

clean:
	rm -rf documentation.json elm-stuff/
	rm -rf $(ALL) examples/elm-stuff/
