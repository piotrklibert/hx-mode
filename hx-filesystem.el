;; -*- mode: emacs-lisp; lexical-binding: t -*-
(require 'promise)
(require 'f)


;; Paths and copying content to the shadow hierarchy

;; TODO:  TEMPORARY globals, until better project handling is in place

(defconst hx--project-root
  "/home/cji/portless/lua/awesome-config/haxeshigh/")
(defconst hx--project-shadow
  "/home/cji/portless/lua/awesome-config/haxeshigh/shadow/")
(defconst hx--hxml-file
  "/home/cji/portless/lua/awesome-config/haxeshigh/shadow/main.hxml")


;; TODO: ./shadow -> ./.batllefield (maybe later)
;; (f-relative hx--project-shadow hx--project-root)
;; (f-relative "/opt" hx--project-root) ;=> "../../../../../../opt"


;; (hx-shadow-update-files (buffer-file-name (get-buffer "Volume.hx")))
(cl-defun hx-shadow-update-files (&optional (path (hx--exp-buf-name)))
  (interactive)
  (when (s-starts-with-p hx--project-root path)
    (let ((shadow-path (hx--shadow-path path)))
      (when-let (path (mkdir (f-dirname shadow-path) t))
        (cl-letf* ((inhibit-message t))
          (message "HX: Created path `%s'" path)))
      (f-write (buffer-string) 'utf-8 shadow-path)
      (cl-letf* ((inhibit-message t))
        (message "HX: Updated file `%s'" shadow-path)))))


(defsubst hx--relative-path (path)
  (f-relative (f-expand path) hx--project-root))


(cl-defun hx--shadow-path (path)
  (f-join hx--project-shadow (hx--relative-path path)))


;; (hx--unshadow-path hx--hxml-file)
;; (hx--unshadow-path (f-join hx--project-shadow "src/volume/Volume.hx"))
(defsubst hx--unshadow-path (path)
  (if (s-starts-with-p hx--project-shadow path)
      (s-replace "shadow/" "" path)
    path))



(cl-defun hx--exp-buf-name (&optional (buf (current-buffer)))
  (f-expand (buffer-file-name buf)))



(provide 'hx-filesystem)
