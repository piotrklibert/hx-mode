;; -*- mode: emacs-lisp; lexical-binding: t -*-


(require 'hx-compiler)


(let ((start-time (float-time)))
  (promise-then
   (hx--query-async
    (hx-query-command
     "~/portless/lua/awesome-config/haxeshigh/shadow/src/volume/Volume.hx"
     "type"
     "626"))
   (lambda (res) (message "+ %s %s" res (- (float-time) start-time)))
   (lambda (err) (message "- %s %s" err (- (float-time) start-time)))))

;; (process-live-p haxe-eldoc--in-progress)
;; (kill-process haxe-eldoc--in-progress)

;; (with-output-to-string
;;   (with-current-buffer standard-output
;;     (call-process "echo" nil t nil "a" "b")))


;; (shell-command-to-string "echo fuck")


;; (let ((promise
;;        (promise-then
;;            (haxe--make-client-interaction haxe--query-command-example)
;;          #f(message "+ %s" %)
;;          #f(message "err -- %s" %))))
;;   (message "-- %s" (process-live-p haxe--prev))
;;   (if (= 0 (random 2))
;;       (kill-process (hx-interaction-process promise)))
;;   ;; (message "-- %s" (process-live-p haxe--prev))
;;   )

;; (let* ((buffer (get-buffer-create "*hx*"))
;;        (interaction (make-hx-interaction :buffer buffer :promise (promise-reject "a") :cmd haxe--query-command-example)))
;;   (promise-then (hx-interaction-promise interaction)
;;     #f(message "+++++ %s" %)
;;     #f(message "err -- %s" %)))

;; (-> (haxe--make-client-interaction haxe--query-command-example)
;;   (promise-then #f(message "+ %s" %) #f(message "- %s" %)))





(provide 'hx--tmp)
