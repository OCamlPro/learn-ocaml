all: static build

DUNE = dune
DUNE_ARGS = --profile=release

build-deps:
	opam install . --deps-only --locked

.PHONY: build
build:
	@${DUNE} build ${DUNE_ARGS}

.PHONY: static
static:
	@${MAKE} -C static

.PHONY: doc
doc:
	@${DUNE} build ${DUNE_ARGS} @doc

.PHONY: install
install: static doc
	@${DUNE} install ${DUNE_ARGS}

uninstall:
	@${DUNE} uninstall

static/dune:
	@${MAKE} -C static dune

.PHONY: travis docker-images publish-docker-images

# Generates up-to-date translation template for lang % from the sources
LANGS = $(patsubst translations/%.po,%,$(wildcard translations/*.po))
translations/$(LANGS:=.pot):
	@for f in $(LANGS); do echo >> translations/$$f.po; done
	@rm -f translations/*.pot
	@${DUNE} clean ${DUNE_ARGS}
	@DUMP_POT=1 ${DUNE} build ${DUNE_ARGS} -j 1
	@for f in $(LANGS); do \
	  mv translations/$$f.pot translations/$$f.pot.bak; \
	  msguniq translations/$$f.pot.bak > translations/$$f.pot; \
	  rm translations/$$f.pot.bak; \
	done

# Updates existing translations (.po) for the latest source template
update-%-translation: translations/%.pot
	@msgmerge -U translations/$*.po translations/$*.pot
	@rm -f translations/$*.pot

opaminstall: install

REPO ?= demo-repository

testrun: build install
	rm -rf www/css
	learn-ocaml build --repo $(REPO) -j1
	rm -rf www/css
	ln -s ../static/css www
	learn-ocaml serve

docker-images: Dockerfile learn-ocaml.opam
	@rm -rf docker
	@git clone . docker
	@cp Dockerfile docker
	@docker build -t learn-ocaml-compilation --target compilation docker
	@docker build -t learn-ocaml --target program docker
	@docker build -t learn-ocaml-client --target client docker
	@echo "Use with 'docker run --rm -v \$$PWD/sync:/sync -v \$$PWD:/repository -p PORT:8080 learn-ocaml -- ARGS'"

VERSION = $(shell opam show ./learn-ocaml.opam -f version)

publish-docker-images: docker-images
	docker tag learn-ocaml ocamlsf/learn-ocaml:$(VERSION)
	docker tag learn-ocaml ocamlsf/learn-ocaml:dev
	docker tag learn-ocaml ocamlsf/learn-ocaml:latest
	docker image push ocamlsf/learn-ocaml:$(VERSION)
	docker image push ocamlsf/learn-ocaml:dev
	docker image push ocamlsf/learn-ocaml:latest

clean:
	@${DUNE} clean
	-rm -f translations/$*.pot
	@${MAKE} -C static clean
	-rm -rf www
	-find . -name "*~" -delete

travis: # From https://stackoverflow.com/questions/21053657/how-to-run-travis-ci-locally
	BUILDID="build-$$RANDOM";					\
	INSTANCE="travisci/ci-garnet:packer-1512502276-986baf0";	\
	docker run --name $$BUILDID -dit $$INSTANCE /sbin/init &&	\
	docker exec -it $$BUILDID bash -l

.PHONY: static-binaries
static-binaries:
	./scripts/static-build.sh
