;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'my-utils)

(require 'promise)
(require 'short-lambda)
(require 'hx-filesystem)
(require 'hx-cli)
(require 'hx-legacy)


;; NOTE: go to ~/portless/lua/awesome-config/haxeshigh/shadow/ and start display
;; server manually for now:
;; haxe --wait 9999 -v -D log ./main.hxml



(defconst hx--service-available-types
  '("toplevel" ; also the default
    "position" ; go to definition
    "usage"    ; see references
    "type"     ; type at point-min
    "package"
    "module-symbols"
    "signature"
    "diagnostics"
    "statistics"))


(defconst hx--client-command-example   ; temporary, replace with proper tests
  (hx-query-command
   "~/portless/lua/awesome-config/haxeshigh/src/volume/Volume.hx"
   "type"
   "629"))

;; (promise-then (hx--query-async hx--client-command-example) (k-part (message "+++ %s")))


;; TODO:
;; (defvar hx-slot-freed-hook nil)
;; (add-hook 'hx-slot-freed-hook (lambda ()))
;;
;; (cl-defstruct hx-interactions-pool
;;   interactions pool-size)


(cl-defstruct hx-interaction
  "A then-able object that combines the `promise' with `process' handle and
other metadata. `promise' is meant to be mutable and is updated during every
`promise-then' call; the rest of the fields should only be set once (however,
this is not enforced currently)."
  promise process buffer cmd output state)


(cl-defmethod promise-then ((this hx-interaction) &optional resolve reject)
  (setf (hx-interaction-promise this)
        (promise-then
            (hx-interaction-promise this)
          resolve reject))
  this)


(cl-defun hx-interaction:interrupt (obj)
  (cl-letf* ((inhibit-message t))
    (with-demoted-errors "hx-kill-interaction: %s"
     (kill-process (hx-interaction-process obj)))))


(put 'hx-query 'lisp-indent-function 1)
(put 'hx--query-async 'lisp-indent-function 1)


(cl-defun hx-query (type &optional async errback)
  "Main function of this module, sends a query about the position at point to
the compiler service and returns either the result directly, or a promise that
will resolve once the started process finishes successfully. If the process
return an error, the promise will be rejected.

This is interactive command and should not be used from Lisp. Use
`hx--query-sync' or `hx--query-async' directly."
  (interactive "sType: ")
  (let* ((path (buffer-file-name (current-buffer)))
         (cmd (hx-query-command path type)))
    (hx-shadow-update-files path)
    (if (not async)
        (hx--query-sync cmd)
      (hx--query-async cmd async errback))))


(cl-defun hx--query-sync (cmd)
  "CMD is a list of strings."
  (cl-letf* ((default-directory hx--project-shadow))
    (shell-command-to-string (s-join " " cmd))))

(cl-defun hx--query-async (cmd &optional callback errback)
  "CMD is a list of strings. CALLBACK and ERRBACK are functions."
  (let ((callback (if (functionp callback) callback #'identity))
        (errback (if (functionp errback) errback #'identity)))
    (promise-then (hx--make-interaction-for-command cmd)
      callback errback)))





(cl-defun my-to-string (arg)
  (format "%s" arg))

(cl-defun hx--make-interaction-for-command (cmd)
  (seq-let (promise process) (hx--make-promised-process cmd)
    (let ((ia (make-hx-interaction :promise promise :process process)))
      (setf (hx-interaction-promise ia)
            (promise-then promise
              (lambda (x)
                ;; (cl-letf* ((inhibit-message t)) (message "c: %s" ?x))
                (setf (hx-interaction-output ia) x)
                (setf (hx-interaction-process ia) nil)
                (setf (hx-interaction-buffer ia) nil)
                (setf (hx-interaction-state ia) 'done)
                (promise-resolve x)
                )
              (lambda (x)
                ;; (cl-letf* ((inhibit-message t)) (message "e: %s" x))
                (setf (hx-interaction-output ia) x)
                (setf (hx-interaction-process ia) nil)
                (setf (hx-interaction-buffer ia) nil)
                (setf (hx-interaction-state ia) 'error)
                (promise-reject x))))
      ia)))


(cl-defun hx--make-promised-process (cmd &optional cwd)
  (setq cmd (mapcar 'my-to-string cmd))
  (let (process)
    (list
     (promise-new
      (lambda (resolve reject)
        (cl-letf*
            ((default-directory (or cwd default-directory))
             (buffer (generate-new-buffer
                      (format "*%s*" (sha1 (s-join "" cmd)))))
             (buf-name (buffer-name buffer))
             (sentinel (hx--make-process-sentinel buffer resolve reject))
             (proc (make-process :command cmd
                                 :name buf-name
                                 :buffer buffer
                                 :stderr nil ; "*err*"
                                 :sentinel sentinel)))
          (setf process proc))))
     process)))


(cl-defun hx--make-process-sentinel (buffer resolve reject)
  (lambda (process event)
    (unwind-protect
        (if (string= event "finished\n")
            (funcall resolve (hx--get-process-output process))
          (funcall reject event))
      (kill-buffer buffer))))


(cl-defun hx--get-process-output (process)
  (with-current-buffer (process-buffer process)
    (buffer-substring-no-properties (point-min) (point-max))))





(cl-defun hx-show-compiler-feedback-at-point (buf)
  (interactive (list (current-buffer)))
  (with-current-buffer buf
    (let ((resps (cl-loop
                  for type in '("toplevel" "position" "type" "usage")
                  collect (hx-query type (k-part (message "%s"))))))
      (promise-then (promise-all resps) #f(message "%s" %)))))

(cl-defun hx--query-toplevel ()
  (interactive)
  (promise-then (hx-query "toplevel" t)
    #f(message "%s" %)))

(with-eval-after-load "haxe-mode"
  (defvar haxe-mode-map)
  (define-key haxe-mode-map (kbd "<f12>") 'hx-show-compiler-feedback-at-point)
  (define-key haxe-mode-map (kbd "<f11>") 'hx--query-toplevel)
  (define-key haxe-mode-map (kbd "<menu>") 'hx-get-completions)
  )


(provide 'hx-compiler)
