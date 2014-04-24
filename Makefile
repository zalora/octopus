VIM= vim
FILE= src/Octopus.hs

GHC_PACKAGE_PATH_SCRIPT= $(shell env | grep '^export GHC_PACKAGE_PATH' | sed 's/.*$$(//g; s/)$$//g')
HDEVTOOLS_ARGS= -g -isrc -g -itest


shell:
	nix-shell --command 'exec zsh'

# hack to make hdevtools work inside nix-shell
hdevtools: $(PACKAGE_CONF)
	-pkill 'hdevtools'
	 test $$IN_NIX_SHELL -eq 1
	 env GHC_PACKAGE_PATH=$$($(GHC_PACKAGE_PATH_SCRIPT)) hdevtools check $(HDEVTOOLS_ARGS) $(FILE)

$(PACKAGE_CONF):
	cabal configure
