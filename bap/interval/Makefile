BUILD_DIR = _build
TEST_SRCS ::= $(shell ls *.ml)
TEST_OBJS = $(addprefix $(BUILD_DIR)/, $(TEST_SRCS:.ml=.cmo))
TEST_INTFS = $(addprefix $(BUILD_DIR)/, $(TEST_SRCS:.ml=.cmi))
TEST_NATIVES = $(addprefix $(BUILD_DIR)/, $(TEST_SRCS:.ml=.cmx))
OCAML_PKGS = -package regular -package ppx_bap -package core_kernel -package bap-knowledge -package bap-core-theory -package bap-main -package bap -package bap-primus
PLUGIN_NAME = uarch_checker.plugin

BAPBUILD_JOB_SLOTS=4

# for building ocaml tags
BAP_MASTER_REPO=/Users/mkf727/repos/bap
Z_PLUGIN_LIB_BASE=/Users/mkf727/.opam/bap-alpha/lib/zarith
JANESTREET_CORE_BASE=/Users/mkf727/.opam/bap-alpha/lib/core

COMP_SIMP_TEST_DIR = comp-simp-test-cases

.PHONY: clean bap-plugin libna_aesni libna_argon2id libna_aead libna_crypto_sign release debug

debug: bap-plugin

release: $(PLUGIN_NAME)

bap-plugin: *.ml
	bapbuild -pkg z3 -pkg bap-primus -j $(BAPBUILD_JOB_SLOTS) $(PLUGIN_NAME)

libna_argon2id: debug
	@echo -n "Starting analysis of argon2id"
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")
	-bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.crypto_pwhash_argon2id.csv \
	--uarch-checker-config-file=crypto_pwhash.uarch_checker.config \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--no-optimization --bil-optimization=0 \
	../../test/libna/bap_ld_scalar_mult.o > libna.crypto_pwhash_argon2id.debug.log 2>&1
	@echo -n "Done with analysis of argon2id"
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")

libna_aesni: debug
	@echo -n "Starting analysis of aesni "
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")
	-bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.crypto_aesni.csv \
	--uarch-checker-target-function=crypto_aead_aes256gcm_encrypt \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--no-optimization --bil-optimization=0 \
	../../test/libna/bap_ld_scalar_mult.o > libna.crypto_aead_aes256gcm_encrypt.debug.log 2>&1
	@echo -n "Done with analysis of aesni "
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")

libna_crypto_sign: debug
	@echo -n "Starting analysis of crypto_sign "
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")
	-bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.crypto_sign_ed25519.csv \
	--uarch-checker-config-file=crypto_sign.uarch_checker.config \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--no-optimization --bil-optimization=0 \
	../../test/libna/bap_ld_scalar_mult.o > libna.crypto_sign.debug.log 2>&1
	@echo -n "Done with analysis of crypto_sign "
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")

libna_aead: debug
	@echo -n "Starting analysis of chacha20poly1305 "
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")
	-bap --plugin-path=. --pass=uarch-checker \
	--uarch-checker-output-csv-file=./libna.aead_chacha20poly1305.csv \
	--uarch-checker-target-function=crypto_aead_chacha20poly1305_encrypt \
	--uarch-checker-ss \
	--uarch-checker-cs \
	--no-optimization --bil-optimization=0 \
	../../test/libna/bap_ld_scalar_mult.o > libna.crypto_aead_chacha20poly1305_encrypt.debug.log 2>&1
	@echo -n "Done with analysis of chacha20poly1305 "
	@echo $$(TZ='America/Los_Angeles' date -- +"%T %Z")

tags: $(TEST_SRCS)
	-ctags -e --etags-include=$(BAP_MASTER_REPO) --etags-include=$(Z_PLUGIN_LIB_BASE) --etags-include=$(JANESTREET_CORE_BASE) -R $^

$(PLUGIN_NAME): $(BUILD_DIR)/$(PLUGIN_NAME)
	ln -sf $^ $@

$(BUILD_DIR)/$(PLUGIN_NAME): $(BUILD_DIR)/uarch_checker.cmxs $(BUILD_DIR)/uarch_checker.cma
	bapbundle pack -name uarch_checker -main $(BUILD_DIR)/uarch_checker.cmxs -main $(BUILD_DIR)/uarch_checker.cma -provides uarch_checker $@

$(BUILD_DIR)/uarch_checker.cmxs: $(BUILD_DIR)/uarch_checker.cmxa
	ocamlfind ocamlopt -shared -linkall -ccopt -L -ccopt $(BUILD_DIR) $< -o $@

$(BUILD_DIR)/uarch_checker.cmxa: $(TEST_NATIVES)
	ocamlfind ocamlopt -a -thread $(OCAML_PKGS) $(TEST_NATIVES) -o $@

$(BUILD_DIR)/uarch_checker.cma: $(TEST_OBJS)
	ocamlfind ocamlc -a -custom -thread $(OCAML_PKGS) $(TEST_OBJS) -o $@

$(BUILD_DIR)/%.cmo: %.ml | $(BUILD_DIR)
	ocamlfind ocamlc -c -thread -I $(BUILD_DIR) $(OCAML_PKGS) -o $@ $<

$(BUILD_DIR)/%.cmx: %.ml | $(BUILD_DIR)
	ocamlfind ocamlopt -c -O3 -thread -I $(BUILD_DIR) $(OCAML_PKGS) -o $@ $<

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

depends: $(BUILD_DIR)/.depends

$(BUILD_DIR)/.depends: $(TEST_SRCS) | $(BUILD_DIR)
	cp $(TEST_SRCS) $(BUILD_DIR)
	ocamlfind ocamldep -nocwd -I $(BUILD_DIR) $(addprefix $(BUILD_DIR)/, $(TEST_SRCS)) > $@

include $(BUILD_DIR)/.depends

clean:
	-ocamlbuild -clean
	-rm *.plugin
	-rm -r _build/
	-rm *.cm[olax]
	-rm *.cmx[as]
	-rm .depends
	-$(MAKE) -C $(COMP_SIMP_TEST_DIR) clean
