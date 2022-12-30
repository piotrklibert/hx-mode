;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'lsp-mode)

(defconst lsp-haxe--haxe-ls-command
  (list "node" "/home/cji/portless/haxe-language-server/bin/server.js"))

(add-to-list 'lsp-language-id-configuration '(haxe-mode . "haxe"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection lsp-haxe--haxe-ls-command)
  :priority -1
  :activation-fn (lsp-activate-on "haxe")
  :server-id 'haxe-ls))

(provide 'lsp-haxe)
