#!/bin/sh

set -e

CURRENT_WIN32_EMACS=/c/emacs/bin/emacs
OLD_WIN32_EMACS=/usr/local/emacs-20.7/bin/emacs.exe
CURRENT_XEMACS=/c/Program\ Files/XEmacs/XEmacs-21.4.13/i586-pc-win32/xemacs.exe

for i in \
	"$CURRENT_WIN32_EMACS" \
	"$OLD_WIN32_EMACS" \
	"$CURRENT_XEMACS"
  do
  echo -n $i ...
  "$i" -batch -load tests.el
  "$i" -batch -load efn-test.el
  "$i" -batch -load info-test.el
  echo $i tests passed.
done

echo All tests passed.
