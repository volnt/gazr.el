(require 'ert)
(require 'gazr)

(ert-deftest parse-makefile-targets ()
  (should (equal (gazr--parse-makefile-targets ".PHONY: foo\nfoo:\n") (list "foo")))
  (should (equal (gazr--parse-makefile-targets "") (list)))
  (should (equal (gazr--parse-makefile-targets ".PHONY: foo foo\n") (list "foo")))
  (should (equal (gazr--parse-makefile-targets ".PHONY: foo") (list "foo")))
  (should (equal (gazr--parse-makefile-targets ".PHONY: foo bar lol\n") (list "foo" "bar" "lol"))))
