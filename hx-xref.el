;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'xref)
(require 'hx-compiler)


(defun hx-xref-backend-function ()
  (when (eq major-mode 'haxe-mode)
    (list 'my-haxe)))


(cl-defmethod xref-backend-identifier-at-point ((_backend (head my-haxe)))
  ;; NOTE: we don't use this value, Haxe compiler works with line-col pos only.
  (xref-backend-identifier-at-point 'elisp))


(cl-defmethod xref-backend-definitions ((_backend (head my-haxe)) symbol)
  (let ((pos (car (hx--format-position (hx-query "position" nil)))))
    (list (xref-make symbol (xref-make-file-location
                             (car pos) (cdr pos) 0)))))


(cl-defmethod xref-backend-references ((_backend (head my-haxe)) symbol)
  (cl-loop for pos in (hx--format-position (hx-query "usage" nil))
           collect (xref-make symbol (xref-make-file-location
                                      (car pos) (cdr pos) 0))))

;; TODO: WTF is this?
(cl-defmethod xref-backend-identifier-completion-table ((_backend (head my-haxe)))
  (list 'fuck))


(cl-defun hx--format-position (data)
  "Parse returned XML and return a list of path and line numbers."
  (cl-assert (cl-typep data 'string))
  (let* ((positions (-> data battle-haxe-get-xml-root car (xml-get-children 'pos)))
         (positions (mapcar #f(nth 2 %) positions)))
    (cl-loop for pos in positions
             for (a b) = (-take 2 (s-split ":" pos))
             collect (cons (hx--unshadow-path a) (string-to-number b)))))


(provide 'hx-xref)
