.PHONY: test coverage runex1 runex2

test:
	nosetests -v test/

coverage:
	nosetests --with-coverage --cover-package=pymop --cover-erase \
	--cover-html --cover-html-dir=cover

runex1:
	python app.py test/data/1616681p22.measure3.cands.astrom /tmp/test

runex1debug:
	python app.py test/data/1616681p22.measure3.cands.astrom /tmp/test --debug

runex2:
	python app.py test/data/1616687p10.measure3.cands.astrom /tmp/test

