.PHONY: local_test vospace_test test_all coverage

local_test:
	nosetests -v tests/ --exclude test_vospace

vospace_test:
	nosetests -v tests/test_vospace/

test_all:
	nosetests -v tests/

coverage:
	nosetests --with-coverage --cover-package=ossos --cover-erase \
	--cover-html --cover-html-dir=cover
