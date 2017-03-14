build:
	corebuild -I src m2048.native


clean:
	corebuild -clean


run:
	./m2048.native


console:
	corebuild -I src list_z.byte
	utop -init ./.ocamlinit


.PHONY: test
test:
	corebuild -I src -I test -pkg oUnit all_tests.native
	./all_tests.native
