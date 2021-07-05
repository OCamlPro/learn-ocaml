.PHONY: icons

all: icons dune

FILES = $(wildcard \
  js/*.js\
  js/ace/*.js\
  js/jszip/*.js\
  *.html\
  icons/*.svg\
  icons/*.gif\
  icons/*.ico\
  fonts/*.woff\
  css/*.css\
) $(shell find js/mathjax ! -type d)

ALWAYS:
file-list: icons ALWAYS
	@echo ${FILES} >$@

icons:
	@${MAKE} -C icons

dune: file-list
	@echo '(install' >$@
	@echo ' (section share)' >>$@
	@echo ' (package learn-ocaml)' >>$@
	@echo ' (files' >>$@
	@$(foreach f,$(FILES),echo '  ($f as ${addprefix www/,$f})' >>$@;)
	@echo ' )' >>$@
	@echo ')' >>$@

clean:
	@${MAKE} -C icons clean
	rm -f dune
