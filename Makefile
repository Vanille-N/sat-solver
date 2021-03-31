# Add binary names to BINARIES as you progress through the project:
# they can be problem encoders or SAT solvers; in both cases you must
# include the extension TODO

BINARIES_NOSKEL=
BINARIES=latin naive $(BINARIES_NOSKEL)

all: $(BINARIES) doc

OCAMLOPT = ocamlopt -I src

# Targets for compiling both problem encoders and SAT solvers

%: src/dimacs.cmx src/hex.cmx src/%.cmx
	$(OCAMLOPT) $+ -o $@

# Testing problem encodings to SAT using minisat

N=10
test_latin: latin
	./latin p $(N)
	minisat problem.cnf output.sat ; ./latin s $(N)
PROBLEM=problems/0/simple1
test_pingouins: pingouins
	./pingouins p $(PROBLEM)
	minisat problem.cnf output.sat ; ./pingouins s $(PROBLEM)
PENALTY=0
tests_pingouins: pingouins
	for i in problems/$(PENALTY)/* ; do \
	  make PROBLEM=$$i PENALTY=$(PENALTY) test_pingouins ; \
	done

# Testing the SAT solver

PROVER=./twl
in_test: all
	@for i in tests/SAT/* ; do \
	  echo -n "$$i... " ; \
	  $(PROVER) $$i output.sat ; \
	  grep -v UNSAT output.sat > /dev/null || exit 1 ; done
	@for i in tests/UNSAT/* ; do \
	  echo -n "$$i... " ; \
	  $(PROVER) $$i output.sat ; \
	  grep UNSAT output.sat > /dev/null || exit 1 ; done
test: all
	@echo Timing tests with minisat...
	@time --output=tests/minisat.time --format=%U \
	  make PROVER=minisat in_test > /dev/null
	@cat tests/minisat.time
	@echo Timing tests with $(PROVER)...
	@time --output=tests/prover.time --format=%U make in_test
	@cat tests/prover.time
	@m=`cat tests/minisat.time` ; p=`cat tests/prover.time` ; \
	  echo -n "Ratio: " ; echo "$$p / $$m" | bc

# Cleaning, documentation, code skeleton

clean:
	rm -f src/*.cmx src/*.o src/*.cmi
	rm -f $(BINARIES)

doc:
	ocamldoc -d html/ -stars -html src/dimacs.mli src/hex.mli

.PHONY: skel
skel:
	rm -rf skel skel.test
	mkdir -p skel/src skel/html
	cat Makefile | sed -e 's/BINARIES_NOSKEL=
	  > skel/Makefile
	cp -r problems/ skel/
	cp src/dimacs.ml* src/hex.ml* src/latin.ml src/naive.ml skel/src/
	cp src/tile_*.ml skel/src/
	cp html/style.css skel/html/
	cp -r tests skel/
	cp -r skel/ skel.test/
	make -C skel.test

# Generic OCaml compilation targets

%.cmx: %.ml
	$(OCAMLOPT) -c $<
%.cmi: %.mli
	$(OCAMLOPT) -c $<

-include .depends
.depends: Makefile $(wildcard src/*.ml src/*.mli)
	ocamldep -native -I src $+ > .depends
