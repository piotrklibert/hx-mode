;;; haxe-mode.el --- Major mode for editing Haxe files  -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'js)
(require 'auto-complete)
(require 'hx-eldoc)
(require 'cc-mode)
(require 'cc-fonts)
(require 'cc-langs)
(require 'cc-bytecomp)
(require 'compile)
(require 'groovy-mode)

;;; Code:

;; The language constants are needed when compiling.
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)
    (load "cc-bytecomp" nil t)))

(defface haxe-string-interpolation-face
  '((default :inherit font-lock-constant-face))
  "Face for highlighting annotations in Haxe mode."
  :group 'haxe)


(eval-and-compile
  ;; Tell the language constant system about Haxe and base it on Java.
  (c-add-language 'haxe-mode 'java-mode))

;;; Lexer-level syntax (identifiers, tokens etc).

;; No other operators in identifiers.
(c-lang-defconst c-after-id-concat-ops haxe nil)

;; Conditional compilation prefix.
(c-lang-defconst c-opt-cpp-prefix haxe "\\s *#")

;; No strings in conditional compilation.
(c-lang-defconst c-cpp-message-directives haxe nil)

;; No file name in angle brackets or quotes in conditional compilation.
(c-lang-defconst c-cpp-include-directives haxe nil)


;; i) the syntax of \"'\" must be \"string quote\" (7);
;; ii) the language's value of `c-has-quoted-numbers' must be nil;
;; iii) the language's value of `c-get-state-before-change-functions' may not
;;      contain `c-parse-quotes-before-change';
;; iv) the language's value of `c-before-font-lock-functions' may not contain
;;   `c-parse-quotes-after-change'."

(c-lang-defconst c-before-font-lock-functions
  haxe '(c-depropertize-new-text
         c-after-change-escape-NL-in-string
         c-after-change-mark-abnormal-strings
         c-restore-<>-properties
         c-change-expand-fl-region))

(c-lang-defconst c-get-state-before-change-functions
  haxe '(c-parse-quotes-before-change
         c-before-change-check-unbalanced-strings
         c-before-change-check-<>-operators))

(c-lang-defconst c-single-quotes-quote-strings haxe t)
(c-lang-defconst c-single-quotes-quote-strings haxe t)

;; No macro definition in conditional compilation.
(c-lang-defconst c-opt-cpp-macro-define haxe nil)

;; Conditional compilation directives followed by expressions.
(c-lang-defconst c-cpp-expr-directives
  haxe '("if" "else"))

;; No functions in conditional compilation.
(c-lang-defconst c-cpp-expr-functions
  haxe nil)

;; Haxe operators.
(c-lang-defconst c-operators
  haxe `(
         ;; Preprocessor.
         (prefix "#")
         ;; Standard operators.
         ,@(c-lang-const c-identifier-ops)
         ;; Generics.
         (postfix-if-paren "<" ">")
         ;; Postfix.
         (left-assoc "." "->")
         (postfix "++" "--" "[" "]" "(" ")")
         ;; Unary.
         (prefix "++" "--" "+" "-" "!" "~" "new")
         ;; Multiplicative.
         (left-assoc "*" "/" "%")
         ;; Additive.
         (left-assoc "+" "-")
         ;; Shift.
         (left-assoc "<<" ">>" ">>>")
         ;; Relational.
         (left-assoc "<" ">" "<=" ">=")
         ;; Iteration.
         (left-assoc "...")
         ;; Equality.
         (left-assoc "==" "!=" "===" "!==")
         ;; Bitwise and.
         (left-assoc "&")
         ;; Bitwise exclusive or.
         (left-assoc "^")
         ;; Bitwise or.
         (left-assoc "|")
         ;; Logical and.
         (left-assoc "&&")
         ;; Logical or.
         (left-assoc "||")
         ;; Assignment.
         (right-assoc ,@(c-lang-const c-assignment-operators))
         ;; Exception.
         (prefix "throw")
         ;; Sequence.
         (left-assoc ",")))

;; No overloading.
(c-lang-defconst c-overloadable-operators haxe nil)
(c-lang-defconst c-opt-op-identitier-prefix haxe nil)

;;; Keywords.

;; I will treat types uniformly below since they all start with capital
;; letters.
(c-lang-defconst c-primitive-type-kwds haxe nil)

;; TODO: check double occurrence of enum.
;; Type-introduction is straight forward in Haxe.
(c-lang-defconst c-class-decl-kwds
  haxe '( "class" "interface" "enum" "typedef" "abstract" "extern"))

;; Recognises enum constants.
;; TODO: find a way to also recognise parameterised constants.
(c-lang-defconst c-brace-list-decl-kwds
  haxe '( "enum" ))

;; Keywords introducing declarations where the identifier follows directly
;; after the keyword, without any type.
(c-lang-defconst c-typeless-decl-kwds
  haxe (append '( "function" "var" "final" "macro" "untyped")
               (c-lang-const c-class-decl-kwds)
           (c-lang-const c-brace-list-decl-kwds)))

;; Definition modifiers.
(c-lang-defconst c-modifier-kwds
  haxe '( "private" "public" "static" "override" "macro" "inline" "untyped" "cast"))
(c-lang-defconst c-other-decl-kwds
  haxe nil)

;; Namespaces.
(c-lang-defconst c-ref-list-kwds
 haxe '( "import" "package" "using" "from" "to" "as"))

;; Statement keywords followed directly by a substatement.
(c-lang-defconst c-block-stmt-1-kwds
  haxe '( "do" "else" "try" ))

;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  haxe '( "for" "if" "switch" "while" "catch" ))

;; Statement keywords followed by an expression or nothing.
(c-lang-defconst c-simple-stmt-kwds
  haxe '( "break" "continue" "return" ;"default"
          "new" ))

;; No ';' inside 'for'.
(c-lang-defconst c-paren-stmt-kwds
  haxe nil)

;; Keywords for constants.
(c-lang-defconst c-constant-kwds
  haxe '( "false" "true" "null" ))

;; Keywords for expressions.
(c-lang-defconst c-primary-expr-kwds
  haxe '( "this" "super" ))

(c-lang-defconst c-decl-hangon-kwds
  haxe '( "in" "as"))

;; No other labels.
(c-lang-defconst c-before-label-kwds
  haxe nil)

;; No classes inside expressions.
(c-lang-defconst c-inexpr-class-kwds
  haxe nil)

;; No brace lists inside expressions.
(c-lang-defconst c-inexpr-brace-list-kwds
  haxe nil)

;; All identifiers starting with a capital letter are types.
(c-lang-defconst c-cpp-matchers
  haxe (append
        `((,(rx (group (or "using " "import ")) (group (? (+ (and (+ alpha) "."))) (group (1+ alpha))) (? (group " as ") (group (1+ alpha))) ";")
           (1 font-lock-keyword-face)
           (2 font-lock-type-face)
           )
          ("\\<\\([A-Z][A-Za-z0-9_]*\\)\\>" 1 font-lock-type-face)
          ("\\(@:[[:alnum:]]+\\)" 1 font-lock-keyword-face)

          ;; Stolen from groovy-mode, created by Russel Winder, Jim Morris, and
          ;; Wilfred Hughes. Thanks, guys! :)
          ;; Highlight $foo string interpolation.
          (,(lambda (limit)
              (let ((pattern (rx (not (any "\\"))
                                 (group
                                  "$" (+ (or (syntax word) (syntax symbol))) symbol-end
                                  (? "." (+ (or (syntax word) (syntax symbol))) symbol-end))))
                    res match-data)
                (save-match-data
                  ;; Search forward for $foo and terminate on the first
                  ;; instance we find that's inside a sring.
                  (while (and
                          (not res)
                          (re-search-forward pattern limit t))
                    (let* ((string-delimiter-pos (nth 8 (syntax-ppss)))
                           (string-delimiter (char-after string-delimiter-pos)))
                      (when (groovy--in-string-p)
                        (setq res (point))
                        ;; Set match data to the group we matched.
                        (setq match-data (list (match-beginning 1) (match-end 1)))))))
                ;; Set match data and return point so we highlight this
                ;; instance.
                (when res
                  (set-match-data match-data)
                  res)))
           (0 'haxe-string-interpolation-face t))
          ;; Highlight ${foo} string interpolation.
          (,(lambda (limit)
              (let (res start)
                (while (and
                        (not res)
                        (search-forward "${" limit t))
                  (let* ((string-delimiter-pos (nth 8 (syntax-ppss)))
                         (string-delimiter (char-after string-delimiter-pos))
                         (escaped-p (eq (char-before (- (point) 2)) ?\\)))
                    (when (and (groovy--in-string-p)
                               (not escaped-p))
                      (setq start (match-beginning 0))
                      (let ((restart-pos (match-end 0)))
                        (let (finish)
                          ;; Search forward for the } that matches the opening {.
                          (while (and (not res) (search-forward "}" limit t))
                            (let ((end-pos (point)))
                              (save-excursion
                                (when (and (ignore-errors (backward-list 1))
                                           (= start (1- (point))))
                                  (setq res end-pos)))))
                          (unless res
                            (goto-char restart-pos)))))))
                ;; Set match data and return point so we highlight this
                ;; instance.
                (when res
                  (set-match-data (list start res))
                  res)))
           (0 'haxe-string-interpolation-face t))
          )
        (c-lang-const c-cpp-matchers c)))
;; (rx (group "@:" (+ alnum)) )
;; Generic types.
(c-lang-defconst c-recognize-<>-arglists
  haxe t)

(c-lang-defconst c-recognize-colon-labels
  haxe nil)

;; Fontification degrees.
(defconst haxe-font-lock-keywords-1 (c-lang-const c-matchers-1 haxe)
  "Minimal highlighting for haxe mode.")
(defconst haxe-font-lock-keywords-2 (c-lang-const c-matchers-2 haxe)
  "Fast normal highlighting for haxe mode.")
(defconst haxe-font-lock-keywords-3 (c-lang-const c-matchers-3 haxe)
  "Accurate normal highlighting for haxe mode.")
(defvar haxe-font-lock-keywords haxe-font-lock-keywords-3
  "Default expressions to highlight in haxe mode.")

(defvar haxe-mode-syntax-table nil
  "Syntax table used in HaXe mode buffers.")
(or haxe-mode-syntax-table
    (setq haxe-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table haxe))))


(defvar haxe-mode-abbrev-table nil
  "Abbreviation table used in haxe mode buffers.")

(c-define-abbrev-table 'haxe-mode-abbrev-table
  ;; Keywords that, if they occur first on a line, might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar haxe-mode-map ()
  "Keymap used in haxe mode buffers.")
(if haxe-mode-map
    nil
  (setq haxe-mode-map (c-make-inherited-keymap)))

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

;; Tell compilation-mode how to parse error messages.  You need to set
;; compilation-error-screen-columns to nil to get the right
;; interpretation of tabs.
(add-to-list 'compilation-error-regexp-alist
             '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
               1 2 3))


(defcustom haxe-mode-hook nil
  "*Hook called by `haxe-mode'."
  :type 'hook
  :group 'haxe)

(require 'hx-compiler)
(require 'hx-filesystem)
(require 'hx-xref)


;;;###autoload
(defun haxe-mode ()
  "Major mode for editing Haxe code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `haxe-mode-hook'.

Key bindings:
\\{haxe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)

  (use-local-map haxe-mode-map)
  (set-syntax-table haxe-mode-syntax-table)

  (setq major-mode 'haxe-mode
        mode-name "Haxe"
        local-abbrev-table haxe-mode-abbrev-table
        abbrev-mode t)

  (c-init-language-vars haxe-mode)
  (c-common-init 'haxe-mode) ; See also: `c-basic-common-init'
  (c-update-modeline)
  ;; (c-add-style "haxe" '((c-offsets-alist . ((case-label . 4)))) t)
  ;; (c-add-style "haxe" my-haxe-style t)

  ;; doesn't work sometimes?
  (setq-local js-switch-indent-offset 4)
  (setq-local indent-line-function #'js-indent-line)
  (setq-local indent-region-function nil)
  (setq-local beginning-of-defun-function #'js-beginning-of-defun)
  (setq-local end-of-defun-function #'js-end-of-defun)
  (setq-local c-syntactic-indentation t)
  (setq-local c-basic-offset 4)
  (setq-local js-indent-level 4)

  (run-hooks 'c-mode-common-hook 'haxe-mode-hook)
  (hack-local-variables)
  (add-hook 'after-save-hook #'hx-shadow-update-files nil t)
  (add-hook 'xref-backend-functions #'hx-xref-backend-function nil t)
  (add-hook 'eldoc-documentation-functions #'hx-eldoc-function nil t)
  (hs-minor-mode 1)

  (add-to-list 'sp-sexp-suffix '(haxe-mode regexp ""))
  )

(defun js--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((when (and js-jsx-syntax (not js-jsx--indent-col))
             (save-excursion (js-jsx--indentation parse-status))))
          ((and (eq (char-after) ?#)
                (save-excursion
                  (forward-char 1)
                  (looking-at-p cpp-font-lock-keywords-source-directives)))
           0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--chained-expression-p))
          ((js--ctrl-statement-indentation))
          ((js--multi-line-declaration-indentation))
          ((nth 1 parse-status)
	   ;; A single closing paren/bracket should be indented at the
	   ;; same level as the opening statement. Same goes for
	   ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (if (or (not js-indent-align-list-continuation)
                     (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                     (save-excursion (forward-char) (js--broken-arrow-terminates-line-p)))
                 (progn ; nothing following the opening paren/bracket
                   (skip-syntax-backward " ")
                   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (js--maybe-goto-declaration-keyword-end parse-status)
                   (let* ((in-switch-p (unless same-indent-p
                                         (looking-at ".*\\_<switch\\_>")))
                          (same-indent-p (or same-indent-p
                                             (and switch-keyword-p
                                                  in-switch-p)))
                          (indent
                           (+
                            (cond
                             ((and js-jsx--indent-attribute-line
                                   (eq js-jsx--indent-attribute-line
                                       (line-number-at-pos)))
                              js-jsx--indent-col)
                             (t
                              (current-column)))
                            (cond (same-indent-p 0)
                                  (continued-expr-p
                                   (+ (* 2 js-indent-level)
                                      js-expr-indent-offset))
                                  (t
                                   (+ js-indent-level
                                      (pcase (char-after (nth 1 parse-status))
                                        (?\( js-paren-indent-offset)
                                        (?\[ js-square-indent-offset)
                                        (?\{ js-curly-indent-offset))))))))
                     (if in-switch-p
                         (+ indent js-switch-indent-offset)
                       indent)))
               ;; If there is something following the opening
               ;; paren/bracket, everything else should be indented at
               ;; the same level.
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t (prog-first-column)))))

;; (modify-syntax-entry ?' "\"" haxe-mode-syntax-table)

;; (add-to-list 'sp-sexp-suffix '(haxe-mode regexp ";"))
;; (setq sp-sexp-suffix nil)

;; (modify-syntax-entry ?. "w" haxe-mode-syntax-table)
;; (modify-syntax-entry ?. "." haxe-mode-syntax-table)

;; switch between file(s) and the compiled output (a'la "go to implementation")

;; (defconst my-haxe-style
;;   '("java" (c-offsets-alist . ( (case-label . +))))
;;   "My Haxe Programming Style")

;; (add-hook 'haxe-mode-hook
;;   (function (lambda () (c-add-style "haxe" my-haxe-style t))))
;; (add-hook 'haxe-mode-hook
;;           (function
;;            (lambda ()
;;              (setq tab-width 4)
;;              (setq indent-tabs-mode t)
;;              (setq fill-column 80)
;;              (local-set-key [(return)] 'newline-and-indent))))



(provide 'haxe-mode)
;;; haxe-mode.el ends here
