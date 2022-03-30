;;; Package --- gazr.el
;;;
;;; Commentary:
;;; Gazr.el can launch Gazr tasks on your project.
;;; It will search a readable Makefile in the current directory and all parent directories.

;;; Code:

(require 'transient)

(defgroup gazr nil
  "Controlling gazr Makefile from Emacs."
  :group 'tools)

(transient-define-prefix  gazr ()
  "Launch gazr targets."
  ["Gazr Targets"
   ("i" "init"          (lambda () (gazr--launch "init")))
   ("b" "build"         (lambda () (gazr--launch "build")))
   ("s" "style"         (lambda () (gazr--launch "style")))
   ("t" "test"          gazr-test)
   ("r" "run"           (lambda () (gazr--launch "run")))]
  [("q" "quit"          transient-quit-one)])

(transient-define-prefix gazr-test ()
  "Launch gazr test targets."
  ["Gazr Tests Targets"
   ("t" "test"          (lambda () (gazr--launch "test")))
   ("u" "unit"          (lambda () (gazr--launch "test-unit")))
   ("i" "integration"   (lambda () (gazr--launch "test-integration")))
   ("f" "functional"    (lambda () (gazr--launch "test-functional")))]
  [("q" "quit"          transient-quit-one)])

(defun gazr--find-makefile (&optional path)
  "Search a readable Makefile in current directory and all parent directories from PATH."
  (let ((path (or path (setq path (buffer-file-name))))
        (parent (file-name-directory (directory-file-name (expand-file-name path))))
        (makefile (concat (file-name-directory path) "Makefile")))
    (if (string= path "/")
        nil
      (if (file-readable-p makefile)
          makefile
        (gazr--find-makefile parent)))))

(defun gazr--launch (target)
  (let ((command
         (format "cd %s; make %s\n" (file-name-directory (gazr--find-makefile)) target)))
      (compile command)))

(provide 'gazr)
;;; gazr.el ends here
