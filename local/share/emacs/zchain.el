;;; Import zchain ssh-agent environment settings

(require 'cl)

;; Extract the environment variable name from an environment
;; setting, e.g. (get-env-var "SSH_AGENT_PID=1234") => "SSH_AGENT_PID"
(defun get-env-var (e)
  (first (split-string e "=")))

;; Replace environment variable v in env
(defun replace-env-var (env v)
               (substitute-if v 
                              (lambda (x) (equal (get-env-var x) (get-env-var v))) 
                              env))

(defun update-environment (new-vars)
  (setq process-environment (reduce 'replace-env-var new-vars :initial-value process-environment)))

(setq comint-password-prompt-regexp "\\(\\([Oo]ld \\|[Nn]ew \\|Enter \\|Bad \\|^\\)[Pp]assword\\|pass ?phrase\\).*?:\\s *\\'")
;; Turn off password echo for interactive shells
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Update the ssh-agent environment vars if they're available
(if (file-readable-p "~/.ssh/ssh-agent-pinfo")
    (update-environment (split-string (shell-command-to-string "cut -f1 '-d;' < ~/.ssh/ssh-agent-pinfo")))
  )

(provide 'zchain)
