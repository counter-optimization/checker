BUILD_DIR = _build
TEST_SRCS ::= $(shell ls *.ml)
TEST_OBJS = $(addprefix $(BUILD_DIR)/, $(TEST_SRCS:.ml=.cmo))
TEST_INTFS = $(addprefix $(BUILD_DIR)/, $(TEST_SRCS:.ml=.cmi))
TEST_NATIVES = $(addprefix $(BUILD_DIR)/, $(TEST_SRCS:.ml=.cmx))
OCAML_PKGS = -pkg ocamlgraph -pkg memtrace -pkg dolog -pkg z3 -pkg splay_tree
PLUGIN_NAME = uarch_checker.plugin

BAPBUILD_JOB_SLOTS=4

# for building ocaml tags
BAP_MASTER_REPO=/Users/mkf727/repos/bap
Z_PLUGIN_LIB_BASE=/Users/mkf727/.opam/bap-alpha/lib/zarith
JANESTREET_CORE_BASE=/Users/mkf727/.opam/bap-alpha/lib/core

COMP_SIMP_TEST_DIR = comp-simp-test-cases

LIBNA=../../test/libna/libna_jammed_together.o

.PHONY: clean bap-plugin libna_aesni libna_argon2id libna_aead libna_crypto_sign release debug

release: bap-plugin

debug: bap-plugin

bap-plugin: *.ml
	 bapbuild $(OCAML_PKGS) -j $(BAPBUILD_JOB_SLOTS) $(PLUGIN_NAME)

global_check: debug
	# --uarch-checker-no-symex
	# 
	@echo -n "Starting checking of libsodium (global) at "
	@echo $$(TZ='America/Los_Angeles' date -- +"%F %T %Z")
	-bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.checker.output.csv \
	--uarch-checker-config-file=libsodium.uarch_checker.config \
	--uarch-checker-no-symex \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--uarch-checker-symex-profiling-output-file=./symex-profiling-data.csv \
	--no-optimization --bil-optimization=0 \
	$(LIBNA) > libna.debug.log 2>&1 
	@echo -n "Done with analysis of argon2id at "
	@echo $$(TZ='America/Los_Angeles' date -- +"%F %T %Z")

tags: $(TEST_SRCS)
	-ctags -e --etags-include=$(BAP_MASTER_REPO) --etags-include=$(Z_PLUGIN_LIB_BASE) --etags-include=$(JANESTREET_CORE_BASE) -R $^

clean:
	-ocamlbuild -clean
	-rm *.plugin
	-rm -r _build/
	-rm *.cm[olax]
	-rm *.cmx[as]
	-rm .depends
	-$(MAKE) -C $(COMP_SIMP_TEST_DIR) clean
