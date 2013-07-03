.PHONY: test coverage

test:
	nosetests -v tests/

coverage:
	nosetests --with-coverage --cover-package=ossos --cover-erase \
	--cover-html --cover-html-dir=cover
