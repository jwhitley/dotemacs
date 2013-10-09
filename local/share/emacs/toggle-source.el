(provide 'toggle-source)

(defvar toggle-source-h-exts '("h" "hh" "hpp" "h++")
  "Extensions to search to find a corresponding header file.")

(defvar toggle-source-c-exts '("c" "cc" "cpp" "c++" "m")
  "Extensions to search to find a corresponding source file.")

(defvar toggle-source-default-h-ext "h"
  "*The default file extension to be used toggle-c-source-files when a header file doesn't already exist.")

(defvar toggle-source-default-c-ext "cpp"
  "*The default file extension to be used by toggle-c-source-files when a source file doesn't already exist.")
(make-variable-buffer-local 'toggle-source-default-c-ext)

(defun toggle-c-source-files ()
  "Switch between C/C++ header and source files.
When called in a buffer with example.c switches to file example.h from the
directory where example.c lives, and vice versa.  Understands various
common C/C++ file extensions including h, hh, hpp, h++, c, cc, cpp, c++."
  (interactive)
  (let ((current-name (buffer-file-name))
        base-name
        new-name
        ext
        search-exts)
    ;; If the current buffer is not visiting a file, signal an error
    (unless (stringp current-name)
      (error "No file associated with the buffer %s" (buffer-name)))
    ;; Try to match the 
    (unless (string-match "\\(.+\\.\\)\\([^.]+\\)" current-name)
      (error "Couldn't make sense of C/C++ file name, %s" current-name))
    ;; Grab the base name and extension
    (setq base-name (replace-match "\\1" nil nil current-name))
    (setq ext (replace-match "\\2" nil nil current-name))

    ;; Set search-exts, the set of extensions to search 
    (cond ((member ext toggle-source-h-exts) (setq search-exts toggle-source-c-exts))
          ((member ext toggle-source-c-exts) (setq search-exts toggle-source-h-exts))
          (t (error "Couldn't make sense of this buffer's extension, %s" ext))
          )

    (unless (catch 'found-name
          (setq ext (car search-exts))
          (while ext
            (setq new-name (replace-match (format "\\1%s" ext) nil nil current-name))
            ;; Attempt to open corresponding file
            (if (file-readable-p new-name)
                (throw 'found-name t))
            (setq ext (car search-exts))
            (setq search-exts (cdr search-exts))))
      ;; The file doesn't already exist.  Set new-name based upon the default file extension
      ;; for header/source files as appropriate.
      (setq new-name (replace-match (format "\\1%s" (if (member ext toggle-source-h-exts) toggle-source-default-h-ext toggle-source-default-c-ext))
                                    nil nil current-name)))
    (find-file new-name)))
