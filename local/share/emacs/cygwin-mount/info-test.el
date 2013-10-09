;; Make sure we can run info.

(load-file "cygwin-mount.el")
(cygwin-mount-activate)
(setenv "INFOPATH")
(info)
