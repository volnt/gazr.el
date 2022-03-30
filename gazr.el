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
   ("i" "init"          (lambda () (interactive) (gazr--launch "init")))
   ("s" "style"         (lambda () (interactive) (gazr--launch "style")))
   ("c" "complexity"    (lambda () (interactive) (gazr--launch "complexity")))
   ("f" "format"        (lambda () (interactive) (gazr--launch "format")))
   ("t" "test"          gazr-test)
   ("r" "run"           (lambda () (interactive) (gazr--launch "run")))
   ("w" "watch"         (lambda () (interactive) (gazr--launch "watch")))
   ("b" "build"         (lambda () (interactive) (gazr--launch "build")))]
  [("q" "quit"          transient-quit-one)])

(transient-define-prefix gazr-test ()
  "Launch gazr test targets."
  ["Gazr Tests Targets"
   ("t" "test"          (lambda () (interactive) (gazr--launch "test")))
   ("u" "unit"          (lambda () (interactive) (gazr--launch "test-unit")))
   ("f" "functional"    (lambda () (interactive) (gazr--launch "test-functional")))
   ("i" "integration"   (lambda () (interactive) (gazr--launch "test-integration")))
   ("s" "security-sast" (lambda () (interactive) (gazr--launch "security-sast")))]
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

(defun gazr--extract-words-first-letters (string)
  "Extract the first letters of each word composing the STRING.
Words are separated by a - (dash)."
  (let ((letters (cl-loop for word in (split-string string "-") if (not (equal word "")) collect (substring word 0 1))))
    (if (equal (length letters) 0)
        nil
        (string-join letters ""))))

(defun gazr--transient-children-setup (_)
   (cl-loop
    for target in (gazr--find-makefile-targets (gazr--find-makefile))
    collect (car (transient--parse-child
             'foobar
             (list
              (gazr--extract-words-first-letters target)
              target
              (lambda () (interactive) (gazr--launch target)))))))

(transient-define-prefix foobar ()
  "Foobar"
  ["Foo yay"
   :setup-children gazr--transient-children-setup])

(defun gazr--launch (target)
  "Launch a gazr TARGET in *compilation* buffer."
  (interactive)
  (let ((command
         (format "cd %s; make %s\n" (file-name-directory (gazr--find-makefile)) target)))
      (compile command)))

(provide 'gazr)
;;; gazr.el ends here
