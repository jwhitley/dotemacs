;;; tests.el --- unit tests for cygwin-mount.el

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: Eric Hanchrow <offby1@blarg.net>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To run these tests:

;;  ./run-all-tests.sh

;; Exit status will be 0 if all tests passed; nonzero otherwise.

;;; Code:

(load-file "./cygwin-mount.el")

(defun mount-output (new windows-dir cygwin-dir)
  "Simulate the output from the Cygwin `mount' command.  NEW
  determines whether the output resembles that from mount.cc versions
  1.11 and later."
  (format 
   (if new
       "%s on %s type %s (%s)\n"
     "%-18s  %-18s  %-11s  %s\n")  
   windows-dir cygwin-dir "user" "binmode"))

(defun assert= (expected actual)
  (when (not (string-equal expected actual))
    (message "expected %S; got %S" expected actual)
    (kill-emacs 1)
    ))

;; Test CYGWIN-MOUNT-PARSE-ONE-LINE by feeding it various pairs of
;; Windows and Cygwin directories, simulating the corresponding
;; outputs of both old and new `mount' commands, parsing those
;; outputs, and checking that the parsed values are identical to the
;; inputs.

;; Use mapcar instead of the more-appropriate mapc because emacs 20.7
;; doesn't have the latter.
(mapcar (function (lambda (arglist)
                    (let* ((expected-win (car  arglist))
                           (expected-cyg (cadr arglist))
                           (new-actuals (cygwin-mount-parse-one-line (funcall 'mount-output t   expected-win expected-cyg)))
                           (old-actuals (cygwin-mount-parse-one-line (funcall 'mount-output nil expected-win expected-cyg)) ))
                      (assert= expected-win (car old-actuals))
                      (assert= expected-win (car new-actuals))
                      (assert= expected-cyg (cdr old-actuals))
                      (assert= expected-cyg (cdr new-actuals))
                      )))
        (apply 'append
               ;; this mess generates two pieces of test data from one
	       ;; list.  For example, ("\\cat" "/dog") becomes
	       ;; (("c:\\cat" "/dog") ("\\\\foo\\bar\\cat" "/dog"))
	       (mapcar 
                (lambda (lst)
                  (let ((win (car lst))
                        (cyg (cadr lst)))
                    `((,(concat "c:"           win) ,cyg)
                      (,(concat "\\\\foo\\bar" win) ,cyg))
                    ))
                '(("\\cygwin" "/")
                  ;("\\probably never happen " "/fred")
                  ("" "/c")
                  ("\\be on \\top" "/")
                  ("\\foo bar baz" "/foo bar/baz")
                  )))) 
