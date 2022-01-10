# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all

all : vowel.native vowel_func.o printbig.o

# "make microc.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

vowel.native :
	opam config exec -- \
	rm -f *.o
	ocamlbuild -use-ocamlfind -pkgs llvm.bitreader vowel.native
	gcc -c vowel_func.c
	cc -emit-llvm -o vowel_func.bc -c vowel_func.c -Wno-varargs
#used to be clang in the last line

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
	rm -f *.o *.output vowel_func.bc

# Testing the "printbig" example

vowel_func : vowel_func.c
	cc -o vowel_func -DBUILD_TEST vowel_func.c

# Building the tarball

TESTS = \
  aaaa add1 arith1 arith2 arith3 fib float1 float2 float3 for1 for2 func1 \
  func2 func3 func4 func5 func6 func7 func8 func9 gcd2 gcd global1 \
  global2 global3 hello if1 if2 if3 if4 if5 if6 local1 local2 ops1 \
  ops2 printbig var1 var2 while1 while2 declassn lex incr intarr incrstr strequality \
  stringnotequal str-intersect stringsub slice strlen

FAILS = \
  assign1 assign2 assign3 dead1 dead2 expr1 expr2 expr3 float1 float2 \
  for1 for2 for3 for4 for5 func1 func2 func3 func4 func5 func6 func7 \
  func8 func9 global1 global2 if1 if2 if3 nomain printbig printb print \
  return1 return2 while1 while2

TESTFILES = $(TESTS:%=test-%.vwl) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.vwl) $(FAILS:%=fail-%.err)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags vowel.ml vowelparse.mly \
	README scanner.mll semant.ml testall.sh \
	vowel_func.c vowel_func.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

vowel.tar.gz : $(TARFILES)
	cd .. && tar czf vowel/vowel.tar.gz \
		$(TARFILES:%=microc/%)
