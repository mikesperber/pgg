SHELL = /bin/sh
BATCH_IMAGE = batch.image
INTERACTIVE_IMAGE = cogen.image
prefix = /usr/local
SCHEME48 = scheme48 -i /home/sperber/soft/scheme48/scheme48.image
INSTALL_DATE = install -c
INTERACTIVE_HEAPSIZE = 4000000
BATCH_HEAPSIZE = 6000000
BATCH_ENTRYPOINT = cogen-main

all: $(BATCH_IMAGE)

cogen_packages = escapes signals
batch_packages = handle i/o conditions big-scheme
cogen-files = auxiliary.scm \
	cogen-abssyn.scm \
	cogen-cps.scm \
	cogen-ctors.scm \
	cogen-dexp.scm \
	cogen-direct-syntax.scm \
	cogen-direct.scm \
	cogen-driver.scm \
	cogen-env.scm \
	cogen-eq-flow.scm \
	cogen-exp.scm \
	cogen-library.scm \
	cogen-load.scm \
	cogen-oca.scm \
	cogen-record.scm \
	cogen-scheme.scm \
	cogen-skeleton.scm \
	shift-reset.scm
batch_files = command-line.scm cogen-batch.scm

$(BATCH_IMAGE) : $(cogen_files) $(batch_files)
	(echo ",batch on"; \
	 echo ",bench on"; \
	 echo ",flush source maps"; \
	 echo ",open $(cogen_packages)"; \
	 echo ",load cogen-load.scm"; \
	 echo ",open $(batch_packages)"; \
	 echo ",load $(batch_files)"; \
	 echo ",flush"; \
	 echo ",collect"; \
	 echo ",dump $(BATCH_IMAGE) \"(cps-mcogen)\""; \
	 echo ",exit" ) \
	| $(SCHEME48) -h $(BATCH_HEAPSIZE)

