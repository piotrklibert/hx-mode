;; -*- mode: emacs-lisp; lexical-binding: t -*-

(require 'promise)
(require 'short-lambda)
(require 'hx-filesystem)
(require 'hx-cli)
(require 'hx-legacy)


;; (locate-file "haxe-mode" load-path (get-load-suffixes))

(defconst hx--service-available-types
  '("toplevel" ;; also the default
    "position" "usage" "package" "type" "module-symbols" "signature"
    "diagnostics" "statistics"))


(defconst hx--client-command-example   ; temporary, replace with proper tests
  (hx-query-command
   "~/portless/lua/awesome-config/haxeshigh/src/volume/Volume.hx"
   "type"
   "626"))


(cl-defstruct hx-interaction
  "A then-able object that combines the `promise' with `process' handle and
other metadata. `promise' is meant to be mutable and is updated during every
`promise-then' call; the rest of the fields should only be set once (however,
this is not enforced currently)."
  promise process buffer cmd)


(cl-defmethod promise-then ((this hx-interaction) &optional resolve reject)
  (setf (hx-interaction-promise this) (promise-then
                                          (hx-interaction-promise this)
                                        resolve reject))
  this)


(cl-defun hx-kill-interaction (obj)
  (kill-process (hx-interaction-process obj)))


(put 'hx-query 'lisp-indent-function 1)
(put 'hx--query-async 'lisp-indent-function 1)


(cl-defun hx-query (type &optional async errback)
  "Main function of this module, sends a query about the position at point to
the compiler service and returns either the result directly, or a promise that
will resolve once the started process finishes successfully. If the process
return an error, the promise will be rejected."
  (interactive "sType: ")
  (let* ((path (buffer-file-name (current-buffer)))
         (cmd (hx-query-command path type)))
    (hx-shadow-update-files path)
    (if (not async)
        (hx--query-sync cmd)
      (hx--query-async cmd async errback))))


(cl-defun hx--query-sync (cmd)
  (cl-letf* ((default-directory hx--project-shadow))
    (shell-command-to-string (s-join " " cmd))))


(cl-defun hx--query-async (cmd &optional callback errback)
  (let ((callback (if (functionp callback) callback #'identity))
        (errback (if (functionp errback) errback #'identity)))
    (promise-then (hx--make-client-interaction cmd)
      callback errback)))


(cl-defun hx--make-client-interaction (cmd)
  (let* ((buf (generate-new-buffer "*haxe*"))
         (cleanup (lambda (trans)
                    (lambda (data)
                      (kill-buffer buf)
                      (funcall trans data)))))
    (promise-then
        (hx--make-client-process-with-buf buf cmd)
      (funcall cleanup 'identity)
      (funcall cleanup 'error))))


(cl-defun hx--make-client-process-with-buf (buf cmd)
  (let
      ((interaction (make-hx-interaction)))
    (setf (hx-interaction-promise interaction)
          (promise-new
           (lambda (resolve reject)
             (cl-letf*
                 ((default-directory hx--project-shadow)
                  (name (buffer-name buf))
                  (sentinel (hx--make-sentinel-function buf resolve reject))
                  (proc (make-process :name name :buffer buf :command cmd
                                      :sentinel sentinel)))
               (setf (hx-interaction-process interaction) proc)
               (setf (hx-interaction-buffer interaction) buf)))))
    interaction))


(cl-defun hx--make-sentinel-function (buffer resolve reject)
  (lambda (process event)
    (cl-assert (eq (process-buffer process) buffer))
    (if (string= event "finished\n")
        (with-current-buffer buffer
          (funcall resolve (buffer-string)))
      (funcall reject event))))


(cl-defun hx-show-compiler-feedback-at-point ()
  (interactive)
  (with-current-buffer (get-buffer "Volume.hx")
    (let ((resps (cl-loop
                  for type in '("position" "type" "module-symbols")
                  collect (hx-query type #f(message "%s" %)))))
      (promise-then (promise-all resps) #f(message "%s" %)))))


(with-eval-after-load "haxe-mode"
  (defvar haxe-mode-map)
  (define-key haxe-mode-map (kbd "<f12>") 'hx-show-compiler-feedback-at-point))


(provide 'hx-compiler)
