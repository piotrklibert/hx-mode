;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'hx-filesystem)
(require 'hx-legacy)


;; Process command-line assembly


(cl-defun hx-query-command (path type &optional (pos nil))
  (hx--client-full-command
   (hx--format-display-arg
    (hx--relative-path path)
    (or pos (battle-haxe-get-byte-pos (point)))
    type)))


(cl-defun hx--format-display-arg (rel-path pos type)
  (format "%s@%s@%s" rel-path pos type))


(cl-defun hx--client-full-command (pos &optional cwd hxml)
  (list
   "/usr/local/bin/haxe"
   "--connect" "9999"
   "--cwd" (or cwd hx--project-shadow)
   "--display" pos
   (or hxml hx--hxml-file)))
;; (hx--client-full-command 123)


(provide 'hx-cli)
