SHELL = /bin/sh
BATCH_IMAGE = batch.image
INTERACTIVE_IMAGE = cogen.image
prefix = /usr/local
SCHEME48 = scheme48
INSTALL_DATE = install -c
INTERACTIVE_HEAPSIZE = 4000000
BATCH_HEAPSIZE = 6000000
BATCH_ENTRYPOINT = cogen-main

all: $(BATCH_IMAGE)

cogen_packages = escapes big-scheme
batch_packages = signals handle i/o conditions extended-ports
cogen_base_files = auxiliary.scm \
	cogen-env.scm \
	cogen-abssyn.scm \
	cogen-scheme.scm \
	cogen-oca.scm \
	cogen-skeleton.scm \
	cogen-eq-flow.scm \
	cogen-library.scm \
	cogen-residual.scm \
	cogen-driver.scm

cogen_cps_files = cogen-cps.scm
cogen_ds_files = cogen-direct-syntax.scm
cogen_combinator_files = $(cogen_ds_files)

cogen_files = $(cogen_base_files) $(cogen_combinator_files)

batch_files = tiny-format.scm fname.scm command-line.scm cogen-batch.scm

gambit_shift_reset = shift-reset-r4rs.scm
gambit_generic_syntax = cogen-ctors-defmacro.scm cogen-record-defmacro.scm

s48_shift_reset = shift-reset.scm
s48_generic_syntax = cogen-ctors.scm cogen-record.scm


cogen-load-s48.scm : Makefile
	(echo "(load \"$(s48_shift_reset)\")" ; \
	 for f in $(s48_generic_syntax) ; do \
		echo "(load \"$$f\")" ; \
	 done ; \
	 echo "(load \"pp.scm\")"; \
	 for f in $(cogen_base_files) ; do \
		echo "(load \"$$f\")" ; \
	 done ; \
	 for f in $(cogen_combinator_files) ; do \
		echo "(load \"$$f\")" ; \
	 done \
	) > $@

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
	 echo ",open $(cogen_packages)"; \
	 echo ",load cogen-load-s48.scm"; \
	 echo ",open $(batch_packages)"; \
	 echo ",load $(batch_files)"; \
	 echo ";; ,flush"; \
	 echo ",collect"; \
	 echo ",dump $(BATCH_IMAGE) \"(cps-mcogen)\""; \
	 echo ",exit" ) \
	| $(SCHEME48) -h $(BATCH_HEAPSIZE)

