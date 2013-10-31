;;;;;; EMACS SETTINGS ;;;;;;

(defun weak-load-file (file)
  (if (file-exists-p file) (load-file file)))

(defun prepend-to-path (dirname)
  (let* ((path (or (getenv "PATH") ""))
         (dir (expand-file-name dirname)))
    (if (not (string-match (regexp-quote dir) path))
        (progn
          (setenv "PATH" (concat dir path-separator path))
          (push dir exec-path)))))

;; Ensure ~/local/bin is on our local PATH.  Some startup environments
;; (e.g. Emacs under X11) won't inherit the full zsh path.
;; 
(prepend-to-path "~/local/bin")

;; Also ensure that ~/local/bin is on our remote PATH when
;; used with TRAMP.
(require 'tramp)
;; tramp-sh won't exist on Emacsen prior to v24
(condition-case nil
    (require 'tramp-sh)         
  (error nil))

(add-to-list 'tramp-remote-path "~/local/bin")

;;;; BEGIN el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq el-get-sources
 '((:name evil
    :after (progn (evil-mode 1)))))

 (setq my-packages
       (append 
        '(undo-tree)
        (mapcar 'el-get-source-name el-get-sources)))

;; install new packages and init already installed packages
(el-get 'sync my-packages)

;;;; END el-get

(add-hook 'Info-mode-hook		; After Info-mode has started
          (lambda ()
    	    (setq Info-additional-directory-list Info-default-directory-list)
            ))

(add-to-list 'load-path "~/local/share/emacs")
(add-to-list 'load-path "~/local/share/emacs/magit")
(add-to-list 'load-path "~/local/share/emacs/tabbar")
(add-to-list 'load-path "~/local/share/emacs/yaml-mode")
(add-to-list 'load-path "~/local/share/emacs/php-mode")
(add-to-list 'load-path "~/local/share/emacs/coffee-mode")

(when (eq system-type 'windows-nt)
  (add-to-list 'load-path "~/local/share/emacs/cygwin-mount")
)

;; Load configuration (if any) specific to this system; not version-controlled.
(weak-load-file "~/.emacs.d/local.el")

;; apache-mode configuration
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; full-ack configuration
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(autoload 'magit-status "magit" nil t)

;; CoffeeScript mode
(require 'coffee-mode)

; color-theme is obsoleted by Emacs themes as of 23.2 
; (aka "Emacs 24")
(if (and (fboundp 'version<) (version< emacs-version "23.2"))
    (progn
      (require 'color-theme-autoloads "color-theme-autoloads")
      (setq color-themes nil)
      (require 'themes)))

(setq default-buffer-file-coding-system 'mule-utf-8-unix)

;;; Transparency settings (Compiz)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(90 85))
(add-to-list 'default-frame-alist '(alpha 90 85))

(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 85))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Windows-only settings
(when (eq system-type 'windows-nt)
  (setq shell-file-name "C:/cygwin/bin/zsh.exe")

  (server-start)

  (defun w32-restore-frame ()
    "Restore a minimized frame"
    (interactive)
    (w32-send-sys-command 61728))

  (defun w32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (w32-send-sys-command 61488))

  (require 'cygwin-mount)
  (cygwin-mount-activate)

  ;; Force the pathname of the Windows starting directory to be
  ;; normalized by Emacs.  This ensures, for example, that the home
  ;; directory will correctly be recognized as "~/..." on startup.
  (cd default-directory)
)

;;; Utility functions
(defun reveal ()
  "Show the current file's working directory in the system GUI file browser."
  (interactive)
  (cond ((eq system-type 'windows-nt) (start-process "Reveal" nil "cmd.exe" "/c" "start" "."))
        ((eq system-type 'cygwin32) (shell-command "cygstart ."))
        ((eq system-type 'darwin) (shell-command "open ."))))

(setq-default ispell-program-name "aspell")

(defun recolor ()
  "Change the default face's background to distinguish this Emacs instance."
  (interactive)
;  (set-face-attribute 'default nil :background "black")
;  (set-face-attribute 'fringe nil :background "#46350A")
  (set-face-attribute 'default nil :background "grey20")
  (set-face-attribute 'fringe nil :background "grey10")
)

;;;;;; MODES AND MODE SETTINGS ;;;;;;;

;;; Mode settings
(if (and (boundp 'tool-bar-mode) (functionp 'tool-bar-mode))
   (tool-bar-mode -1))
(if (boundp 'menu-bar-mode)
   (menu-bar-mode -1))

(delete-selection-mode t)
(line-number-mode t)
(column-number-mode t)

(when (member (downcase (system-name)) '("jwhitley" "devserver"))
  (load-library "p4"))

;; Miscellaneous enablings
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Force query on exit (C-x C-c) from xemacs
;(defun query-exit-from-xemacs ()
;  (yes-or-no-p "Do you really want to exit? "))
;(setq kill-emacs-query-functions
;      (append kill-emacs-query-functions (list 'query-exit-from-xemacs)))

(require 'pabbrev)
;; Disable errors from pabbrev when the buffer is read-only
(setq pabbrev-read-only-error nil)

(windmove-default-keybindings 'meta)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.\\(md\\|markdown\\)$" . markdown-mode) auto-mode-alist))
(defun my-markdown-mode-hook ()
  (setq word-wrap t))
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;; cmd-mode
(autoload 'cmd-mode "cmd-mode" "Mode for editing cmd files" t)
(add-to-list 'auto-mode-alist '("\\.\\(cmd\\|bat\\)$" . cmd-mode))

;; wikipedia mode
(autoload 'wikipedia-mode "wikipedia-mode.el" 
  "Major mode for editing documents in Wikipedia markup." t)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . wikipedia-mode))

;; ruby mode
(require 'ruby-mode)
(defun ruby-eval-buffer()
  (interactive)
  "Evaluate the buffer with Ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(defun my-ruby-mode-hook ()
  (setq standard-indent 2)
  (define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Rakefile\\|\\.rake\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\(\\.lock\\)?$" . ruby-mode))

(setq-default c-basic-offset 3)
(setq-default indent-tabs-mode nil)
;; Except for sh-mode
(setq-default sh-basic-offset 2)
(setq-default perl-indent-level 2) ;; And perl mode
(setq-default js-indent-level 2) ;; and js mode

(require 'toggle-source)

;; c-mode hook
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; Turn on hungry delete mode in C/C++ modes
             (c-toggle-hungry-state 1)
             (local-set-key "\M-o" 'toggle-c-source-files)
             ;; This stupidly defaults to '+':
             (c-set-offset 'innamespace 0)
             ))

;; nginx mode
(require 'nginx-mode)

;; php-mode
(autoload 'php-mode "php-mode" 
  "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-hook 'php-mode-hook
          '(lambda ()
             (set (make-local-variable 'tab-width) 3)
             (set (make-local-variable 'c-basic-offset) 3)
             (set (make-local-variable 'indent-tabs-mode) nil)
             (c-set-offset 'block-open' - )
             (c-set-offset 'block-close' 0 )
             ))

;; python-mode
(add-to-list 'auto-mode-alist '("\\(SConstruct\\|Sconscript\\)$" . python-mode))

;; Balance mode for managing finances
(autoload 'balance-mode "balance")
(add-to-list 'auto-mode-alist '("\\.bal$" . balance-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; Ignore '*.templ' suffixes when determining the auto-mode
(add-to-list 'auto-mode-alist '("\\.templ$" nil t))

;;;;;; KEYMAP SETTINGS ;;;;;;;

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-\M-r" 'isearch-backward)
(global-set-key "\C-ha" 'apropos)
(global-set-key "\C-xg" 'magit-status)

(cond ((eq window-system nil)
       (xterm-mouse-mode)
       (global-set-key [mouse-4] 'scroll-down-line)
       (global-set-key [mouse-5] 'scroll-up-line)))

;;;;;; CUSTOMIZATION ;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("d2673db68d51016c27fe11df59d0e73aa554348f" "abc45303cb3556c3be3a642c209a33d9b0883ffb" default)))
 '(dired-dnd-protocol-alist nil)
 '(grep-command "grep -nHI -e ")
 '(grep-files-aliases (quote (("el" . "*.el") ("ch" . "*.[ch]") ("c" . "*.c") ("h" . "*.h") ("asm" . "*.[sS]") ("m" . "[Mm]akefile*") ("l" . "[Cc]hange[Ll]og*") ("tex" . "*.tex") ("texi" . "*.texi") ("rb" . "*.rb"))))
 '(grep-find-command "find . -type f -print0 | xargs -0 grep -nHI -e ")
 '(grep-find-template "find . <X> -type f <F> -print0 | xargs -0 grep <C> -nHI -e <R>")
 '(inhibit-startup-screen t)
 '(large-file-warning-threshold nil)
 '(ns-antialias-text t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(safe-local-variable-values (quote ((c-mode . linux) (add-log-time-format lambda nil (let* ((time (current-time)) (diff (+ (cadr time) 32400)) (lo (% diff 65536)) (hi (+ (car time) (/ diff 65536)))) (format-time-string "%a %b %e %H:%M:%S %Y" (list hi lo) t))))))
 '(scroll-bar-mode (quote right))
 '(shell-prompt-pattern "^[^#$%>
]*[#$%>]+ *")
 '(tramp-shell-prompt-pattern "\\(?:^\\|\\)[^#$%>
]*[#$%>]+ *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(transient-mark-mode t)
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "ivory" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
 '(cursor ((t (:background "salmon"))))
 '(flymake-errline ((((class color)) (:background "Pink4"))))
 '(flymake-warnline ((((class color)) (:background "LightBlue4"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "gold"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "DarkSeaGreen1"))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "orange"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "seagreen2"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "chartreuse1"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "deep sky blue"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "sky blue"))))
 '(fringe ((((class color) (background dark)) (:background "#46350A")))))

;;;;;; PLATFORM-SPECIFIC CUSTOMIZATION ;;;;;;;

;; Here I override any configuration settings that need to be
;; specified per-platform to look/work correctly on that system-type.

(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :height 98 :family "outline-lucida console" :foundry "unknown" )
       (require 'zchain))
      ;; ((eq system-type 'gnu/linux)
      ;;  (set-face-attribute 'default nil :height 111 :family "Monospace"))
      ((eq system-type 'darwin)
       ;; This defaults to ns-insert-file. #fail
       (global-set-key [ns-drag-file] 'ns-find-file)
       ;; Use GNU Coreutils' version of ls installed via homebrew
       (setq insert-directory-program "gls")
       (set-face-attribute 'default nil :height 160 :weight 'light :family "Source Code Pro")
       (setq ns-pop-up-frames nil))
      (t
       ;; ido-mode
       (ido-mode t)
                                        ; fuzzy matching is a must have
       (setq ido-enable-flex-matching t)))
