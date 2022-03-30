.DEFAULT_GOAL := help

.PHONY: test
test: test-unit ## Launch all the tests (unit, functional and integration)

.PHONY: test-unit
test-unit: ## Launch the unit tests
	emacs -batch -L . -l t/conf.el -l ert -l t/test-gazr.el -f ert-run-tests-batch
	@echo "Unit Testing..."

.PHONY: help
help:  ## Display command usage
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
