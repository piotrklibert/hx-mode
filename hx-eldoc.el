;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'hx-compiler)

(defvar hx-eldoc--in-progress nil)


(cl-defun hx--parse-eldoc (res)
  (cond
   ((s-contains-p "<metadata>" res) (s-replace-regexp "</?metadata>" "" res))
   (t
    (hx-eldoc--parse-type res))))


(cl-defun hx-eldoc--parse-type (res)
  (let*
      ((root (-> res battle-haxe-get-xml-root car))
       (pos (s-split ":" (xml-get-attribute root 'p)))
       (symname
        (apply #'hx--get-symbol-from-buffer-pos
               (->> pos                 ; pos is: '(fname lineno char-range)
                 (-update-at 1 'string-to-number)
                 (-update-at 2 #f(hx--parse-position-range %))))))
    (format "%s: %s" symname (s-trim (nth 2 root)))))


;; (hx--parse-position-range "2-45")
(defun hx--parse-position-range (char-range)
  (->> char-range
    (s-replace "characters " "")
    (s-trim)
    (s-split "-")
    (-map 'string-to-number)))


(defun hx--get-symbol-from-buffer-pos (fname line range)
  (with-current-buffer (get-buffer (f-filename (s-trim fname)))
    (save-excursion
      (goto-char (point-min))
      (let ((beg (line-beginning-position line)))
        (buffer-substring-no-properties
         (+ beg (cl-first range) -1)
         (+ beg (cl-second range) -1))))))




(defconst my-tmp nil)
(defconst my-tmp-next 0)

;; (hx--parse-eldoc my-tmp)

(cl-defun hx-eldoc-function (callback)
  (interactive)
  (let ((interaction (hx-query "type" t)))
    (setq hx-eldoc--in-progress interaction)
    (promise-then interaction
      (lambda (result)
        (when (equal hx-eldoc--in-progress interaction)
          (funcall callback (hx--parse-eldoc result))
          (setq hx-eldoc--in-progress nil)))
      (lambda (err)
        ;; happens when point is over keywords, imports, and other things; and
        ;; also when the process is killed
        (cl-letf* ((inhibit-message t))
          (message "%s" "error or abort"))
        (when (equal hx-eldoc--in-progress interaction)
          (setq hx-eldoc--in-progress nil))
        (promise-reject err)))))


(provide 'hx-eldoc)
