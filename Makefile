# Add binary names to BINARIES as you progress through the project:
# they can be problem encoders or SAT solvers; in both cases you must
# include the extension TODO

BINARIES_NOSKEL=
BINARIES=latin naive arrays greek pingouins wang wang2 twl $(BINARIES_NOSKEL)

all: $(BINARIES) doc

OCAMLOPT = ocamlfind ocamlopt -I src

# Targets for compiling both problem encoders and SAT solvers

%: src/dimacs.cmx src/hex.cmx src/model.cmx src/dll.cmx src/%.cmx
	$(OCAMLOPT) $+ -o $@

# Testing problem encodings to SAT using minisat

N=10
test_latin: latin
	./latin p $(N)
	minisat problem.cnf output.sat ; ./latin s $(N)
test_greek: greek
	./greek p $(N)
	minisat problem.cnf output.sat ; ./greek s $(N)
test_wang: wang
	./wang p $(N)
	minisat problem.cnf output.sat ; ./wang s $(N)
test_wang2: wang2
	./wang2 p $(N)
	minisat problem.cnf output.sat ; ./wang2 s $(N)
PROBLEM=problems/0/simple1
test_pingouins: pingouins
	./pingouins p $(PROBLEM)
	minisat problem.cnf output.sat ; ./pingouins s $(PROBLEM)
PENALTY=0
tests_pingouins: pingouins
	for i in problems/$(PENALTY)/* ; do \
	  make PROBLEM=$$i PENALTY=$(PENALTY) test_pingouins ; \
	done

cairo:
	rm -f src/jr_cairo.{cmx,o,cmi}
	ocamlfind ocamlopt -package cairo2 -linkpkg \
		-I src src/dimacs.cmx src/jr_cairo.ml -o jr_cairo

# Testing the SAT solver

test_dll: src/dll.ml
	@# this is a great hack
	cat src/dll.ml <( echo ";; test ();;" ) | ocaml -stdin

PROVER=./twl
WITNESS=./arrays
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
verify: all
	@echo "Checking correctness of traces"
	@for i in \
		tests/SAT/{flat50-1000.cnf,par8-1-c.cnf,quinn.cnf,zebra_v155_c1135.cnf} \
		tests/UNSAT/{bf1355-075.cnf,hole6.cnf} \
	; do \
		echo "$$i... " ; \
	  	DEBUG=1 $(PROVER) $$i tests/output.sat > tests/prover.trace ; \
		DEBUG=1 $(WITNESS) $$i tests/output.sat > tests/witness.trace ; \
		echo "          <diff>" ; \
		diff tests/prover.trace tests/witness.trace ; \
		echo "          </diff>" ; \
	done

# Cleaning, documentation, code skeleton

clean:
	rm -f src/*.cmx src/*.o src/*.cmi
	rm -f $(BINARIES)
	rm -f *.svg
	rm -f tests/*.{trace,sat,model,time}
	rm -rf html/*.html

doc:
	ocamldoc -d html/ -stars -html src/*.mli

.PHONY: skel cairo clean doc
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
