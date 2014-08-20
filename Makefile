# disable prarallel builds.
.NOTPARALLEL:

action    = install

all:    install

first-install: delete sandbox force-install

configure:  ; $(MAKE) -e target action=configure
build:    ; $(MAKE) -e target action=build
install:  ; $(MAKE) -e target action=install

target: ssh

sandbox:
	cabal sandbox init --sandbox .cabal-sandbox
	cd examples/simple     && cabal sandbox init --sandbox ../../.cabal-sandbox

ssh: sandbox
	cabal $(action)

example-simple: ssh
	cd examples/simple && cabal $(action)