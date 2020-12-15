
MAKEFLAGS += --no-print-directory

# Do not remove this block. It is used by the 'help' rule when
# constructing the help output.
# help:
# help: SILC Makefile help
# help:

# help: help                           - display this makefile's help information
.PHONY: help
help:
	@grep "^# help\:" Makefile | grep -v grep | sed 's/\# help\: //' | sed 's/\# help\://'

# help:
# help: Build
# help: -------

# help: pyclient                       - Build the python client bindings
.PHONY: pyclient
pyclient: SHELL:=/bin/bash
pyclient:
	@bash ./build-scripts/build-python-bindings.sh


# help: deps                           - Make SILC dependencies
.PHONY: deps
deps: SHELL:=/bin/bash
deps:
	@bash ./build-scripts/build_deps.sh


# help: test-deps                      - Make SILC testing dependencies
.PHONY: test-deps
test-deps: SHELL:=/bin/bash
test-deps:
	@bash ./build-scripts/build_test_deps.sh


# help: test-deps-gpu                  - Make SILC GPU testing dependencies
.PHONY: test-deps
test-deps-gpu: SHELL:=/bin/bash
test-deps-gpu:
	@bash ./build-scripts/build_test_deps.sh gpu


# help: build-tests                    - build all tests (C, C++, Fortran)
.PHONY: build-tests
build-tests: build-test-cpp build-test-c build-test-fortran


# help: build-test-cpp                 - build the C++ tests
.PHONY: build-test-cpp
build-test-cpp:
	./build-scripts/build_cpp_tests.sh


# help: build-test-c                   - build the C tests
.PHONY: build-test-c
build-test-c:
	./build-scripts/build_c_tests.sh

# help: build-test-fortran             - build the Fortran tests
.PHONY: build-test-fortran
build-test-fortran:
	./build-scripts/build_fortran_tests.sh

# help: clean-deps                     - remove third-party deps
.PHONY: clean-deps
clean-deps:
	@rm -rf ./third-party


# help: clean                          - remove builds, pyc files, .gitignore rules
.PHONY: clean
clean:
	@git clean -X -f -d


# help: clobber                        - clean, remove deps, builds, (be careful)
.PHONY: clobber
clobber: clean clean-deps


# help:
# help: Style
# help: -------

# help: style                          - Sort imports and format with black
.PHONY: style
style: sort-imports format


# help: check-style                    - check code style compliance
.PHONY: check-style
check-style: check-sort-imports check-format


# help: format                         - perform code style format
.PHONY: format
format:
	@black ./src/python/module/silc ./tests/python/


# help: check-format                   - check code format compliance
.PHONY: check-format
check-format:
	@black --check ./src/python/module/silc ./tests/python/


# help: sort-imports                   - apply import sort ordering
.PHONY: sort-imports
sort-imports:
	@isort ./src/python/module/silc ./tests/python/ --profile black


# help: check-sort-imports             - check imports are sorted
.PHONY: check-sort-imports
check-sort-imports:
	@isort  ./src/python/module/silc ./tests/python/ --check-only --profile black


# help: check-lint                     - run static analysis checks
.PHONY: check-lint
check-lint:
	@pylint --rcfile=.pylintrc ./src/python/module/silc ./tests/python


# help:
# help: Documentation
# help: -------

# help: docs                           - generate project documentation
.PHONY: docs
docs:
	@cd doc; make html


# help:
# help: Test
# help: -------

# help: test                           - Build and run all tests (C, C++, Fortran, Python)
.PHONY: test
test: build-tests
test:
	@python -m pytest -vv ./tests


# help: test-verbose                   - Build and run all tests [verbosely]
.PHONY: test-verbose
test-verbose: build-tests
test-verbose:
	@python -m pytest -vv -s ./tests

# help: test-c                         - Build and run all C tests
.PHONY: test-c
test-c: build-test-c
test-c:
	@python -m pytest -vv -s ./tests/c/

# help: test-cpp                       - Build and run all C++ tests
.PHONY: test-cpp
test-cpp: build-test-cpp
test-cpp:
	@python -m pytest -vv -s ./tests/cpp/


# help: test-py                        - run python tests
.PHONY: test-py
test-py:
	@python -m pytest -vv ./tests/python/

# help: test-fortran                   - run fortran tests
.PHONY: test-fortran
test-fortran: build-test-fortran
	@python -m pytest -vv ./tests/fortran/

# help: testpy-cov                     - run python tests with coverage
.PHONY: testpy-cov
testpy-cov:
	@python -m pytest --cov=./src/python/module/silc/ -vv ./tests/python/
