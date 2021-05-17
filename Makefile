# Add binary names to BINARIES as you progress through the project:
# they can be problem encoders or SAT solvers; in both cases you must
# include the extension TODO

BINARIES=latin naive arrays greek pingouins wang twl jr_test

all: $(BINARIES) doc target

OCAMLOPT = ocamlfind ocamlopt -I src -I target

# Targets for compiling both problem encoders and SAT solvers

%: target/%.cmx
	$(OCAMLOPT) $(shell cat .depargs | grep "^$+" | cut -d: -f2) $+ -o $@

# Testing problem encodings to SAT using minisat

CNF_F=tests/problem.cnf
SAT_F=tests/output.sat

N=10
test_latin: latin
	./latin p $(N)
	minisat $(CNF_F) $(SAT_F) ; ./latin s $(N)
test_greek: greek
	./greek p $(N)
	minisat $(CNF_F) $(SAT_F) ; ./greek s $(N)
test_wang: wang jr_test
	./wang p $(N)
	minisat $(CNF_F) $(SAT_F) ; ./wang s $(N)
	cat tests/solution.txt | ./jr_test
PROBLEM=problems/0/simple1
test_pingouins: pingouins
	./pingouins p $(PROBLEM)
	minisat $(CNF_F) $(SAT_F) ; ./pingouins s $(PROBLEM)
PENALTY=0
tests_pingouins: pingouins
	for i in problems/$(PENALTY)/* ; do \
	  	make PROBLEM=$$i PENALTY=$(PENALTY) test_pingouins || exit 100 ; \
	done
time_pingouins: pingouins
	time for i in `seq 0 5`; do \
		make PENALTY=$$i tests_pingouins || exit 100 ; \
	done

# Testing the SAT solver

test_dll: src/dll.ml
	@# this is a great hack
	cat src/dll.ml <( echo ";; test ();;" ) | ocaml -stdin

PROVER=./twl
WITNESS=./arrays
in_test: all
	@for i in tests/SAT/* ; do \
	  echo -n "$$i... " ; \
	  $(PROVER) $$i $(SAT_F) ; \
	  grep -v UNSAT $(SAT_F) > /dev/null || exit 1 ; done
	@for i in tests/UNSAT/* ; do \
	  echo -n "$$i... " ; \
	  $(PROVER) $$i $(SAT_F) ; \
	  grep UNSAT $(SAT_F) > /dev/null || exit 1 ; done
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
	  	DEBUG=1 $(PROVER) $$i $(SAT_F) > tests/prover.trace ; \
		DEBUG=1 $(WITNESS) $$i $(SAT_F) > tests/witness.trace ; \
		echo "          <diff>" ; \
		diff tests/prover.trace tests/witness.trace ; \
		echo "          </diff>" ; \
	done

# Cleaning, documentation, code skeleton

clean:
	rm -f target/*
	rm -f $(BINARIES)
	rm -f *.svg
	rm -f tests/*.{trace,sat,model,time}
	rm -rf html/*.html

doc:
	ocamldoc -d html/ -stars -html src/*.mli

.PHONY: cairo clean doc 

target:
	mkdir -p target

# Generic OCaml compilation targets

target/%.cmx: src/%.ml
	$(OCAMLOPT) -c $< -o $@
target/%.cmi: src/%.mli
	$(OCAMLOPT) -c $< -o $@

# Generate dependency file for ocamlopt
# Basically black magic
# Input:
#     bar.cmx : \
#         bar.cmi
#     bar.cmi :
#     baz.cmx : \
#         foo.cmx \
#         bar.cmx
#     foo.cmx : \
#         foo.cmi
#     foo.cmi :
# Output:
#     bar.cmx : 
#     baz.cmx : foo.cmx bar.cmx 
#     foo.cmx : 
# Which is usable by the $(shell cat .depargs | grep "$($+ :)" | cut -d: -f2) above
#                                ^this file     ^find left       ^select right
-include .depends
.depends: Makefile $(wildcard src/*.ml src/*.mli)
	ocamldep -native -I src $+ | sed 's,src,target,g' > .depends
	@cat .depends \
	| grep cmx \
	| sed 's,^\(.*\):,*\1:,g' \
	| tr '\n' ' ' \
	| sed 's,\\,,g' \
	| sed 's, \+, ,g' \
	| tr '*' '\n' \
	> .depargs
	@echo "" >> .depargs
