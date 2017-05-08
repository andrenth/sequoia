INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	jbuilder build @install

install:
	jbuilder install $(INSTALL_ARGS)

uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

reinstall: uninstall install

test:
	jbuilder build @runtest

example:
	jbuilder build examples/server.exe

clean:
	rm -rf _build

.PHONY: default install uninstall reinstall clean test
