SHELL = /bin/sh
BATCH_IMAGE = batch.image
INTERACTIVE_IMAGE = pgg.image
COGEN_VERSION = 1.0
DISTRIBUTION = pgg-$(COGEN_VERSION).tar.gz
GENEXT_DISTRIBUTION = genext-$(COGEN_VERSION).tar.gz
prefix = /usr/local
SCHEME48 = scheme48
INSTALL_DATE = install -c
INTERACTIVE_HEAPSIZE = 4000000
BATCH_HEAPSIZE = 6000000
BATCH_ENTRYPOINT = cogen-main

all: $(INTERACTIVE_IMAGE)

distribution: $(DISTRIBUTION)
genext-distribution: $(GENEXT_DISTRIBUTION)

cogen_packages = pgg pgg-residual pgg-library
cogen_generate_packages = pgg signals pretty-print
cogen_specialize_packages = auxiliary pgg-library cogen-specialize
batch_packages = signals handle i/o conditions extended-ports
genext_base_files = \
        auxiliary.scm \
	cogen-gensym.scm \
	cogen-boxops.scm \
	cogen-globals.scm \
	cogen-specialize.scm \
	cogen-library.scm \
	shift-reset.scm \
	cogen-residual.scm \
	cogen-completer.scm \
	cogen-record.scm \
	cogen-memo-standard.scm \
	cogen-ctors.scm
pgg_base_files = \
	cogen-abssyn.scm \
	cogen-construct-genext.scm \
	cogen-driver.scm \
	cogen-effect.scm \
	cogen-env.scm \
	cogen-eq-flow.scm \
	cogen-labset-bylist.scm \
	cogen-macro.scm \
	cogen-oca.scm \
	cogen-scheme.scm \
	cogen-skeleton.scm \
	cogen-terminate.scm \
	cogen-typesig.scm \
	scheme-standard-macros.scm

cogen_base_files = $(genext_base_files) $(pgg_base_files)

cogen_cps_files = cogen-cps.scm
cogen_ds_files = cogen-direct-anf.scm
cogen_combinator_files = $(cogen_ds_files)

genext_files = $(genext_base_files) $(cogen_combinator_files)
cogen_files = $(cogen_base_files) $(cogen_combinator_files)

batch_files = tiny-format.scm fname.scm command-line.scm cogen-batch.scm

gambit_shift_reset = shift-reset-r4rs.scm
gambit_generic_syntax = cogen-ctors-defmacro.scm cogen-record-defmacro.scm

s48_shift_reset = shift-reset.scm
s48_generic_syntax = cogen-ctors.scm cogen-record.scm

genext_config_files = genext-packages.scm
pgg_config_files = pgg-packages.scm
config_files = $(genext_config_files) $(pgg_config_files)
cogen_examples = \
	examples/2lazy-support.scm \
	examples/2lazy.scm \
	examples/app.scm \
	examples/apply.scm \
	examples/cyclic.scm \
	examples/dotprod.scm \
	examples/int.scm \
	examples/list.dat \
	examples/object.scm \
	examples/pm-input.scm \
	examples/pm.scm \
	examples/power.scm \
	examples/pure-arith.scm \
	examples/unify-aux.scm \
	examples/unify.scm
additional_files = Makefile

cogen-load-gambit.scm : Makefile
	(echo "(load \"$(gambit_shift_reset)\")" ; \
	 for f in $(gambit_generic_syntax) ; do \
		echo "(load \"$$f\")" ; \
	 done ; \
	 for f in $(cogen_base_files) ; do \
		echo "(load \"$$f\")" ; \
	 done ; \
	 for f in $(cogen_combinator_files) ; do \
		echo "(load \"$$f\")" ; \
	 done \
	) > $@

$(BATCH_IMAGE) : $(cogen_files) $(batch_files) cogen-load-s48.scm
	(echo ",batch on"; \
	 echo ",bench on"; \
	 echo ";; ,flush source maps"; \
	 echo ",load-package $(cogen_packages)"; \
	 echo ",open $(cogen_generate_packages)"; \
	 echo ",open $(cogen_specialize_packages)"; \
	 echo ",open $(batch_packages)"; \
	 echo ",load $(batch_files)"; \
	 echo ";; ,flush"; \
	 echo ",collect"; \
	 echo ",dump $(BATCH_IMAGE) \"(PGG-$(COGEN_VERSION))\""; \
	 echo ",exit" ) \
	| $(SCHEME48) -h $(BATCH_HEAPSIZE)

$(INTERACTIVE_IMAGE) : $(cogen_files) $(config_files)
	(echo ",bench on"; \
	 echo ",config,load $(config_files)"; \
	 for package in $(cogen_packages) ; do \
	 echo ",load-package $$package"; \
	 done ; \
	 echo ",open $(cogen_generate_packages)"; \
	 echo ",open $(cogen_specialize_packages)"; \
	 echo ",collect"; \
	 echo ",dump $(INTERACTIVE_IMAGE) \"(PGG-$(COGEN_VERSION))\""; \
	 echo ",exit" ) \
	| $(SCHEME48) -h $(INTERACTIVE_HEAPSIZE)

$(DISTRIBUTION): $(cogen_files) $(config_files) $(cogen_examples) $(additional_files)
	tar cvhzf $(DISTRIBUTION) $(cogen_files) $(config_files) $(cogen_examples) $(additional_files)

$(GENEXT_DISTRIBUTION): $(genext_files) $(genext_config_files)
	tar cvhzf $(GENEXT_DISTRIBUTION) $(genext_files) $(genext_config_files)
