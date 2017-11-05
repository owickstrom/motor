PLANTUML=.deps/plantuml.jar

UML_SRCS=$(shell find diagrams -name '*.uml.txt')
UMLS=$(UML_SRCS:diagrams/%.uml.txt=diagrams/%.png)

.PHONY: all
all: $(UMLS)

diagrams/%.png: diagrams/%.uml.txt Makefile diagrams/styles.iuml $(PLANTUML)
	mkdir -p $(shell dirname $@)
	cat $< | java -jar $(PLANTUML) -Idiagrams/styles.iuml -tpng -pipe > $@

$(PLANTUML):
	mkdir -p $(shell dirname $@)
	wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O $@
