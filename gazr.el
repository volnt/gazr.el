;;; Package --- gazr.el
;;;
;;; Commentary:
;;; Custom implementation of goto-line
;;; Read line from minibuffer
;;; Cast it to integer
;;; And move cursor to line

;;; Code:

(require 'transient)

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

(defun gazr--find-makefile (&optional path)
  (let ((path (or path (setq path (buffer-file-name))))
        (parent (file-name-directory (directory-file-name (expand-file-name path))))
        (makefile (concat (file-name-directory path) "Makefile")))
    (if (string= path "/")
        nil
      (if (file-readable-p makefile)
          makefile
        (gazr--find-makefile parent)))))

(defun gazr--launch (target)
  (let ((command (format "cd %s; make %s\n" (file-name-directory (gazr--find-makefile)) target)))
      (compile command)))

(defun gazr-launch-init ()
  (interactive)
  (gazr--launch "init"))

(defun gazr-launch-build ()
  (interactive)
  (gazr--launch "build"))

(transient-define-prefix gazr-test ()
  "Launch gazr test targets."
  ["Gazr Tests Targets"
   ("t" "test"          gazr-launch-test)
   ("u" "unit"          gazr-launch-test-unit)
   ("i" "integration"   gazr-launch-test-integration)
   ("f" "functional"    gazr-launch-test-functional)]
  [("q" "quit"          transient-quit-one)])

(defun gazr-launch-test ()
  (interactive)
  (gazr--launch "test"))

(defun gazr-launch-style ()
  (interactive)
  (gazr--launch "style"))

(defun gazr-launch-test-integration ()
  (interactive)
  (gazr--launch "test-integration"))

(defun gazr-launch-test-unit ()
  (interactive)
  (gazr--launch "test-unit"))

(defun gazr-launch-test-functional ()
  (interactive)
  (gazr--launch "test-functional"))

(defun gazr-launch-run ()
  (interactive)
  (gazr--launch "run"))

(provide 'gazr)
;;; gazr.el ends here
