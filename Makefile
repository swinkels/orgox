# This Makefile is based on the [Eask template Makefile][1].
#
# [1]: https://github.com/emacs-eask/template-elisp/blob/master/Makefile

EMACS ?= emacs
EASK ?= eask

.PHONY: checkdoc clean compile install lint package test

all: clean checkdoc lint test compile package install

# check documentation strings, comment styles and more
#
# checkdoc is part of Emacs but you can find more information on the [CEDET page
# on checkdoc][1].
#
# [1]: https://cedet.sourceforge.net/checkdoc.shtml

checkdoc:
	$(EASK) lint checkdoc --strict

# Clean the entire workspace including the following folders and files:
#   - .eask folder (sandbox)
#   - all .elc files
clean:
	# Remove artifacts of other Makefile rules
	$(EASK) clean all

compile:
	# Compile all your package .el files to .elc
	$(EASK) compile

install:
	# Install package in Eask sandbox (.eask)
	$(EASK) install

# lint package metadata
#
# From [purcell/package-lint][1],
#
# > package-lint detects various issues that may make your package uninstallable
# > or unusable for some users, and it warns about significant deviations from
# > the Elisp coding conventions, such as non-compliant symbol naming, and use
# > of reserved keybindings.
#
# [1]: https://github.com/purcell/package-lint

lint:
	$(EASK) lint package

# This tests that your package can be built correctly before the package
# installation.
package:
	# Create package tarball in `dist` folder
	$(EASK) package

test:
	# Run unit tests
	$(EASK) install-deps --dev
	$(EASK) test ert-runner

# LocalWords:  CEDET Elisp checkdoc eask el emacs purcell shtml
