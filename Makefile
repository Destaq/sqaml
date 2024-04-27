bisect:
	find . -name '*.coverage' | xargs rm -f
	OUNIT_CI=true dune test --instrument-with bisect_ppx --force
	bisect-ppx-report html
	open ./_coverage/index.html

clean:
	rm -rf _coverage
	dune clean
