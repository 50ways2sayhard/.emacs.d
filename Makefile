-include lib/borg/borg.mk

env:
	@printenv > ~/.emacs.d/env
	@echo MU_PATH=`brew --prefix mu` >> ~/.emacs.d/env

update:
	@git submodule update --remote
	@make	clean && make build
	@test -f ~/.emacs.d/etc/borg/autoload/autoload-*.el \
    && rm -r ~/.emacs.d/etc/borg/autoload/autoload-*.el

ifndef BORG_DIR

help helpall::
	$(info )
	$(info Bootstrapping)
	$(info -------------)
	$(info make bootstrap-borg  = make borg and make targets available)
	@printf "\n"

bootstrap-borg:
	@mkdir .git/modules
	@git clone https://github.com/emacscollective/borg lib/borg \
	--separate-git-dir .git/modules/borg
	@cd lib/borg; git symbolic-ref HEAD refs/heads/main
	@cd lib/borg; git reset --hard HEAD

else

helpall::
	$(info Test and fix targets)
	$(info --------------------)
	$(info make codespell-dry   = run codespell, dry run)
	$(info make codespell-fix   = run codespell, write fixes)
	@printf "\n"

codespell-dry:
	@cd lib; codespell \
	  --ignore-words ../etc/codespell/ignore-words \
	  --exclude-file ../etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  etc/codespell/ignore-files | tr "\\n" ",")

codespell-fix:
	@cd lib; codespell --write-changes \
	  --ignore-words ../etc/codespell/ignore-words \
	  --exclude-file ../etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  etc/codespell/ignore-files | tr "\\n" ",")

endif

init: env bootstrap-borg bootstrap
