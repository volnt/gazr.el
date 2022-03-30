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

(defvar gazr-root-targets
  '(("init" .
     ("i" "init"             (lambda () (interactive) (gazr--launch "init"))))
    ("style" .
     ("s" "style"            (lambda () (interactive) (gazr--launch "style"))))
    ("complexity" .
     ("c" "complexity"       (lambda () (interactive) (gazr--launch "complexity"))))
    ("format" .
     ("f" "format"           (lambda () (interactive) (gazr--launch "format"))))
    ("test" .
     ("t" "test"             gazr-test))
    ("run" .
     ("r" "run"              (lambda () (interactive) (gazr--launch "run"))))
    ("watch" .
     ("w" "watch"            (lambda () (interactive) (gazr--launch "watch"))))
    ("build" .
     ("b" "build"            (lambda () (interactive) (gazr--launch "build"))))))

(defvar gazr-test-targets
  '(("test" .
     ("t" "test"             (lambda () (interactive) (gazr--launch "test"))))
    ("test-unit" .
     ("u" "unit"             (lambda () (interactive) (gazr--launch "test-unit"))))
    ("test-functional" .
     ("f" "functional"       (lambda () (interactive) (gazr--launch "test-functional"))))
    ("test-integration" .
     ("i" "integration"      (lambda () (interactive) (gazr--launch "test-integration"))))
    ("security-sast" .
     ("s" "security-sast"    (lambda () (interactive) (gazr--launch "security-sast"))))))

(transient-define-prefix  gazr ()
  "Launch gazr targets."
  ["Gazr Targets"
   :setup-children gazr--transient-root-children-setup]
  [("q" "quit"          transient-quit-one)])

(transient-define-prefix gazr-test ()
  "Launch gazr test targets."
  ["Gazr Test Targets"
   :setup-children gazr--transient-test-children-setup]
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

(defun gazr--parse-makefile-targets (content)
  "Search CONTENT for .PHONY targets and return them as a list of strings."
  (let ((targets (list))
        (start nil))
    (while (string-match "^\.PHONY:" content start)
      (progn
        (setq start (match-end 0))
        (setq targets (append (split-string (substring content (match-end 0) (or (string-match "\n" content (match-end 0))))) targets))))
    (delete-dups targets)))

(defun gazr--find-makefile-targets (makefile)
  "Open MAKEFILE and parse its targets."
  (let ((targets (list)))
    (with-temp-buffer
      (insert-file-contents makefile)
      (gazr--parse-makefile-targets (buffer-string)))))

(defun gazr--transient-root-children-setup (_)
  (cl-loop
   for target in (gazr--find-makefile-targets (gazr--find-makefile))
   if (assoc target gazr-root-targets)
   collect (car (transient--parse-child
                 'gazr
                 (cdr (assoc target gazr-targets))))))

(defun gazr--transient-test-children-setup (_)
  (cl-loop
   for target in (gazr--find-makefile-targets (gazr--find-makefile))
   if (assoc target gazr-test-targets)
   collect (car (transient--parse-child
                 'gazr-test
                 (cdr (assoc target gazr-targets))))))

(defun gazr--launch (target)
  "Launch a gazr TARGET in *compilation* buffer."
  (interactive)
  (let ((command
         (format "cd %s; make %s\n" (file-name-directory (gazr--find-makefile)) target)))
    (compile command)))

(provide 'gazr)
;;; gazr.el ends here
