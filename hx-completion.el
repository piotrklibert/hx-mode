;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'auto-complete)
(require 'hx-compiler)
(require 'cl-lib)
(require 'cl-macs)
(require 'ht)




(defvar hx-ac-candidates '("asd"))

;; (xml-node-attributes)

(cl-defun hx-comp--parse-entry (i)
  (if-let (n (alist-get 'n (cl-second i)))
      n
    (cl-third i)))

(cl-defun hx-ac-candidates ()
  (let ((x (battle-haxe-get-xml-root (hx-query "toplevel"))))
    (cl-loop for i in (xml-get-children (car x) 'i)
             collect (hx-comp--parse-entry i))))


(ac-define-source haxe
  '((candidates . hx-ac-candidates)
    (symbol . "x")))


(defconst hx--keywords-list
  '(continue catch typedef cast throw try switch overload macro public override
    enum inline extends import break for if while false package do using this
    case var final true null private static class in new abstract interface
    untyped default implements else dynamic function return operator extern))

;; (message "%s" (s-join ",\n" (-map #f(format "'%s'" %) hx--keywords-list)))

(ac-define-source haxe-keywords
  '((candidates . hx--keywords-list)
    (cache . t)
    (symbol . "k")))


(cl-defun hx-get-completions ()
  (interactive)
  (promise-then (hx-query "toplevel" t) #'hx-show-completions))

(defconst hx-completion-blacklist
  (mapcar 'symbol-name
          '(run or let sure apply check orGet unsafe toOption run or let sure
            apply check orGet unsafe toOption)))

(cl-defun hx-completion-colorize-type (s)
  (with-current-buffer (get-buffer-create "Nvm.hx")
    (erase-buffer)
    ;; (haxe-mode)
    (insert s)
    (font-lock-ensure (point-min) (point-max))
    (buffer-string) ))

(cl-defun hx-get-completions-annotations (hash)
  (lambda (arg)
    (let* ((hxc (ht-get hash arg))
           (type (hx-completion-colorize-type
                  (or (hx-completion-type hxc) "")))
           (doc (or (hx-completion-doc hxc) ""))
           (padding (s-repeat (max 0 (- 20 (length arg))) " ")))
      (format "%s%s   %s" padding type doc))))


(require 'xml)
(require 'dom)

(cl-defun hx-completion-trim-string (str)
  (s-join " " (--map (s-chop-prefix "* " (s-trim it)) (s-lines (s-trim str)))))


(cl-defun hx-completions-parse-dot-completions (nodes)
  (cl-loop for n in nodes
           collect (make-hx-completion
                    :name (dom-attr n 'n)
                    :kind (dom-attr n 'k)
                    :type (dom-text (dom-by-tag n 't))
                    :doc (hx-completion-trim-string (dom-text (dom-by-tag n 'd))))))
(hx-completions-parse-xml hx-completions-example-list)


(cl-defun hx-completions-parse-ns-completions (nodes)
  (cl-loop for n in nodes collect
           (make-hx-completion
            :name (dom-texts n)
            :kind (dom-attr n 'k)
            :type (dom-attr n 't)
            :doc (hx-completion-trim-string (or (dom-attr n 'd) ""))
            :pkg (dom-attr n 'p))))
(hx-completions-parse-xml hx-completions-example-il)


(cl-defun hx-completions-parse-xml (node)
  (cl-case (dom-tag node)
    (list (hx-completions-parse-dot-completions (dom-non-text-children node)))
    (il   (hx-completions-parse-ns-completions (dom-non-text-children node))))
  )

(cl-defstruct hx-completion
  ""
  name kind type doc pkg)

(cl-defun hx-completion-make-hash (completions-list)
  (let ((hash (ht)))
    (cl-loop for hxc in completions-list
             do (ht-set hash (hx-completion-name hxc) hxc))
    hash))

(cl-defun hx-completions-extra-props (hash)
  (list :annotation-function (hx-get-completions-annotations hash)))


(cl-defun hx-completion-prepare-candidates (candidates pre)
  (cl-labels ((choose-font-face (cand)
                (condition-case err
                    (pcase (hx-completion-kind cand)
                      ("var" 'font-lock-builtin-face)
                      ("method" 'font-lock-keyword-face)
                      (_ nil))
                  (error (message "error: %s" err)))))
    (cl-loop for cand in candidates
             for name = (hx-completion-name cand)
             if (s-starts-with-p pre name)
             collect (propertize name 'face (choose-font-face cand)))))

(cl-defun hx-show-completions (node)
  (let* ((completions-list (hx-completions-parse-xml
                           (battle-haxe-get-xml-root node)))
         (completions-hash (hx-completion-make-hash completions-list))
         (completion-extra-properties (hx-completions-extra-props
                                       completions-hash))
         (dotpos (save-excursion
                   (search-backward "." (line-beginning-position) t)))
         (pre (if dotpos (buffer-substring-no-properties (1+ dotpos) (point)) ""))
         (candidates (hx-completion-prepare-candidates completions-list pre))
         (res (completing-read "> " candidates)))
    (when dotpos
      (delete-region (1+ dotpos) (point)))
    (insert res)))

(provide 'hx-completion)
