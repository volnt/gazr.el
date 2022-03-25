;;; Package --- gazr.el
;;;
;;; Commentary:
;;; Custom implementation of goto-line
;;; Read line from minibuffer
;;; Cast it to integer
;;; And move cursor to line

;;; Code:

(defgroup gazr nil
  "Controlling gazr Makefile from Emacs."
  :group 'tools)

(transient-define-prefix  gazr ()
  "Launch gazr targets."
  ["Gazr Targets"
   ("i" "init"          gazr-launch-init)
   ("b" "build"         gazr-launch-build)
   ("s" "style"         gazr-launch-style)
   ("t" "test"          gazr-test)
   ("r" "run"           gazr-launch-run)]
  [("q" "quit"          transient-quit-one)])

(defun gazr-launch (target)
  (interactive)
  (call-process "make" nil "gazr-process" nil target))

(defun gazr-launch-init ()
  (interactive)
  (gazr-launch "init"))

(defun gazr-launch-build ()
  (interactive)
  (gazr-launch "build"))

(transient-define-prefix gazr-test ()
  "Launch gazr test targets."
  ["Gazr Tests Targets"
   ("t" "test"          gazr-launch-test)
   ("u" "unit"   gazr-launch-test-unit)
   ("i" "integration"   gazr-launch-test-integration)
   ("f" "functional"   gazr-launch-test-functional)]
  [("q" "quit"          transient-quit-one)])

(defun gazr-launch-test ()
  (interactive)
  (gazr-launch "test"))

(defun gazr-launch-style ()
  (interactive)
  (gazr-launch "style"))

(defun gazr-launch-test-integration ()
  (interactive)
  (gazr-launch "test-integration"))

(defun gazr-launch-test-unit ()
  (interactive)
  (gazr-launch "test-unit"))

(defun gazr-launch-test-functional ()
  (interactive)
  (gazr-launch "test-functional"))

(defun gazr-launch-run ()
  (interactive)
  (gazr-launch "run"))

(provide 'gazr)
;;; gazr.el ends here
