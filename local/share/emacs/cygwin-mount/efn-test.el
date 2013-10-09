;; ensure there's no directory named c:/sbin; the test assumes there isn't.

(defvar bogus-dirname "c:/sbin"
  "A directory, expressed in native Windows style, that doesn't exist,
but whose name might be returned from expand-file-name if that
function depends on an old buggy Cygwin.")

(when (file-directory-p bogus-dirname)
  (message "Can't run efn-test since directory %s already exists"
	   bogus-dirname)
  (kill-emacs 0))

;; assert that the return from `(expand-file-name "" "/sbin")' names a
;; directory that exists.

(load-file "cygwin-mount.el")
(cygwin-mount-activate)
(let* ((args (list "" (concat "/" (file-name-nondirectory bogus-dirname))))
       (expanded  (apply 'expand-file-name args))
       (pass (file-directory-p expanded)))
  (when (not pass)
    (message "%S incorrectly returned %S"
	     (cons 'expand-file-name args)
	     expanded)
    (kill-emacs 1)))
