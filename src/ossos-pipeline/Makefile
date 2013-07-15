.PHONY: unit_test integration_test test coverage

unit_test:
	nosetests -v tests/ --exclude test_integration

integration_test:
	nosetests -v tests/test_integration/

test:
	nosetests -v tests/

coverage:
	nosetests --with-coverage --cover-package=ossos --cover-erase \
	--cover-html --cover-html-dir=cover
