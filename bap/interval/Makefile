TEST_SRCS = wrapping_interval.ml test_wrapping_interval.ml test_main.ml
TEST_OBJS = $(TEST_SRCS:.ml=.cmo)
TEST_INTFS = $(TEST_SRCS:.ml=.cmi)
TEST_MAIN = test_main
PLUGIN_NAME = uarch_checker.plugin

COMP_SIMP_TEST_DIR = comp-simp-test-cases

PKGS = bap,core_kernel,stdio,graphlib,bignum,zarith,ounit2

.PHONY : clean test comp-simp-test-cases bap-plugin

bap-plugin : *.ml
	bapbuild $(PLUGIN_NAME)

test : $(TEST_MAIN) comp-simp-test-cases
	./$(TEST_MAIN)

comp-simp-test-cases :
	$(MAKE) -C $(COMP_SIMP_TEST_DIR)

$(TEST_MAIN) : $(TEST_OBJS)
	ocamlfind ocamlc -o test_main -package $(PKGS) -linkpkg $(TEST_OBJS)

$(TEST_OBJS) &: $(TEST_SRCS)
	ocamlfind ocamlc -package $(PKGS) -c $(TEST_SRCS)

clean :
	rm $(TEST_MAIN) || true
	rm $(TEST_OBJS) || true
	rm $(TEST_INTFS) || true
	rm *.plugin || true
	yes | rm -r _build/ || true
	$(MAKE) -C $(COMP_SIMP_TEST_DIR) clean || true
