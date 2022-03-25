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
  ["Targets"
   ("i" "init"          gazr-launch-init)
   ("b" "build"         gazr-launch-build)
   ("t" "test"          gazr-launch-test)
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

(defun gazr-launch-test ()
  (interactive)
  (gazr-launch "test"))

(defun gazr-launch-run ()
  (interactive)
  (gazr-launch "run"))

(provide 'gazr)
;;; gazr.el ends here
