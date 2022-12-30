;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'hx-compiler)

;; add-interaction
;; run-hook-with-args
;; cache positions -> check if range left
;; detecting changes in buffer

;; (locate-file "haxe-mode" load-path (get-load-suffixes))
(defconst my-tmp
  (hx--query-async
      (hx-query-command
       "~/portless/lua/awesome-config/haxeshigh/shadow/src/volume/Volume.hx"
       "type"
       "625")))


(promise-then (hx-interaction-promise my-tmp) (k-part (message "+++ %s")))
(promise--resolve (hx-interaction-promise my-tmp) 22)
(let ((x (promise-then (promise--value 'asd ) (k-part (message "+++ %s")))))
  (run-at-time "1" nil (lambda () (promise-then x (k-part (message "--- %s"))))))
(promise-then (hx-interaction-promise my-tmp) (lambda (x) (message "asadasdas")))

(let ((start-time (float-time)))
  ;; promise-then
  (hx-kill-interaction
   (promise-then
     #f(message "--- %s" %)
     #f(message "--- %s" %)
     ))
  ;;(lambda (res) (message "+ %s %s" res (- (float-time) start-time)))
  ;;(lambda (err) (message "- %s %s" err (- (float-time) start-time)))
  )


(setf my-tmp
      (let ((p (promise:delay 1 42)))
        (prog1 (promise-chain p
                 (then (lambda (x) (message "1:%s - %s" x p) (promise:delay 1 x)))
                 (then (lambda (x) (message "2:%s - %s" x p)))
                 (then (lambda (x) (message "3:%s" x))))
          (run-at-time "4" nil (lambda () (promise-then p
                                       (k-part (message "+++ %s"))
                                       (k-part (message "+++ %s"))))))))


(setq my-tmp (promise--value 1))
(promise-then my-tmp
  (k-part (message "+++ %s")))



(promise-then  my-tmp (k-part (message "+++ %s")))
(promise-then
    (promise-then
      (lambda (x) (message "333") (promise:delay 1 x)))
  (k-part (message "--- %s") ))

(cl-defun hx--make-client-interaction (cmd)
  (let* ((cleanup (lambda (trans)
                    (lambda (data)
                      (kill-buffer buf)
                      (funcall trans data)))))
    (promise-then
        (hx--make-client-process-with-buf buf cmd)
      (funcall cleanup 'identity)
      (funcall cleanup 'error))))


(setf (symbol-function 'hx--make-client-process-with-buf) nil)


(cl-defun process-deferred ()
  )

(default-directory hx--project-shadow)




(promise-then (hx--make-interaction-for-command (list "echo" "$$$"))
  (k-part (message "+++ %s")))

(let ((a (promise-then (hx--make-interaction-for-command (list "bash" "-c" "false"))
           (k-part (message "+++ %s"))
           (k-part (message "--- %s"))
           )))
  (print (hx-interaction-state a))
  (run-at-time "1" nil #f(print (hx-interaction-state a)))
  a
  )

(promise-then
    (promise-chain (promise-reject 'fuck)
      (then (k-part (message "+++ %s")))
      (catch (lambda (x) (print x) (promise-reject x)))
      (catch (k-part (message "--- %s")))
      )
  'print)


(hx--make-promised-process (hx-query-command
                            "/home/cji/portless/lua/awesome-config/haxeshigh/shadow/src/volume/Volume.hx"
                            "type"
                            "626")
                           "/home/cji/portless/lua/awesome-config/haxeshigh/shadow/"
                           )

(cl-letf* ((default-directory "/home/cji/portless/lua/awesome-config/haxeshigh/shadow/"))
  (promise-then
    (k-part (message "+++ %s ---"))
    'my-debug ;(k-part (message "+++ %s ---"))
    ))





(seq-let (a b) (hx--make-promised-process (list "echo" "asd"))
  (process-status b)
  (print (promise-_state a))
  (run-at-time "1" nil #f(print (process-status b)))
  (run-at-time "2" nil #f(print (promise-_value a)))
  (cons a b))



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
